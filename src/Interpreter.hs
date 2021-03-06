{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

-- Without fault-tolerance nor a fixed time period of exexution, the interpreter is easy: acquire a
-- connection to each of the other nodes, use them to send and receive messages when the Program says
-- to, and continue doing so forever. What enables this simplicity is that we have a single input
-- source, the messages we receive, and a single output sink, the messages we send. This allows us to
-- block on the single input source without worrying about any other sources of interference.
-- 
-- With a fixed period of execution, things become a bit more complicated because the input messages
-- and the time-triggered events are both input sources. To reorganize the system so that every piece
-- has a single source again, I run two threads upstream of the interpreter's thread, one which
-- counts the elapsed time and one which waits for events from the endpoint. Both send 'Action's to
-- the interpreter's thread, who now has a single input source of events. Of course, those are now
-- Action events, not messages, so the interpreter's logic has to be extended accordingly.
-- 
-- With fault-tolerance, things become even more complicated. The Program should continue to execute,
-- sending and receiving contributions, with the only difference being that we no longer exchange
-- messages with the nodes which are no longer reachable. At the same time, we should also repeatedly
-- attempt to reconnect to those unreachable nodes, in case the network connectivity is restored. I
-- do this by spawning a reconnection thread each time we're diconnected from one of the other nodes.
-- This reconnection thread repeatedly attempts to reconnect, and informs the interpreter when it
-- succeeds, so that the interpreter can send the latest contribution to the newly-connected node.
module Interpreter (interpret) where

import           Control.Concurrent
import           Control.Distributed.Process.Extras.Time
import           Control.Lens (makeLenses, use, (.=), (%=), (+=))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Sequence (Seq, (|>))
import           Data.Time
import           Data.Void
import           System.Random
import           Text.Printf

import           Config hiding (Command)
import           Control.Concurrent.MyExtra
import           Control.Monad.MyExtra
import           Log
import           Message
import           Network.Transport.MyExtra
import           Network.Transport.TCP.Address
import           Program
import           Text.Parsable


-- While the main interpreter will spend most of its time waiting for messages, the helper threads
-- described above will take care of auxiliary tasks such as making sure we stop after the allocated
-- time expires. Here are the actions which the helpers can ask the interpreter to perform.
data Action
  = AddConnection Address Connection
  | RemoveConnection Address
  | ProcessContribution NodeIndex Contribution
  | StopSendingNow
  | PrintResultNow


data InterpreterState = InterpreterState
  { _activeConnections    :: !(Map Address Connection)
  , _latestContribution   :: !Contribution
  , _canSendContributions :: !Bool
  , _committedMessages    :: !(Seq Message)
  , _scoreMultiplier      :: !Int
  , _previousScore        :: !Double
  , _committedScore       :: !Double
  }

initialInterpreterState :: InterpreterState
initialInterpreterState = InterpreterState
                        { _activeConnections    = mempty
                        , _latestContribution   = mempty
                        , _canSendContributions = True
                        , _committedMessages    = mempty
                        , _scoreMultiplier      = 1
                        , _previousScore        = 0
                        , _committedScore       = 0
                        }

makeLenses ''InterpreterState


-- the interpreter's monad transformer stack: its state, the random number's state, and IO
type M = StateT InterpreterState (StateT StdGen IO)

runM :: Int -> M a -> IO a
runM seed = flip evalStateT (mkStdGen seed)
          . flip evalStateT initialInterpreterState


interpret :: UserProvidedConfig -> UTCTime -> NodeIndex -> Address -> [Address] -> Endpoint -> Program Void -> IO ()
interpret (UserProvidedConfig {..}) startTime myIndex myAddress allAddresses endpoint program = do
    mvar <- newEmptyMVar
    mapM_ (connectLater mvar) peerAddresses
    _ <- forkIO $ timeKeeper mvar
    _ <- forkIO $ runTransportT $ contributionReceiver mvar
    runM mySeed (go mvar program)
  where
    nbNodes :: Int
    nbNodes = length allAddresses
    
    peerAddresses :: [Address]
    peerAddresses = filter (/= myAddress) allAddresses
    
    addressMap :: Map NodeIndex Address
    addressMap = Map.fromList $ zip [0..] allAddresses
    
    indexToAddress :: NodeIndex -> Address
    indexToAddress i = addressMap Map.! i
    
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: MVar Action -> Command a -> MaybeT M a
    go1 _ GetNbNodes                 = return nbNodes
    go1 _ GetMyNodeIndex             = return myIndex
    go1 _ GenerateRandomMessage      = lift . lift $ randomMessage
    go1 _ (BroadcastContribution c') = do
        c <- use latestContribution
        when (c /= c') $ do
          liftIO $ putLogLn configVerbosity 4
                 $ printf "node %s improves to %s"
                          (unparse myAddress)
                          (show c')
          
          -- A surprising optimization: don't send anything yet! With finite bandwidth, flooding the
          -- network with messages will only slow things down. Instead, we'll keep the number of in-
          -- transit messages constant by doing some "ping-pong". We send a message to each of the
          -- other nodes on startup and after reconnecting after a broken connection, and then we
          -- wait until we hear back from them before sending the next message.
          latestContribution .= c'
    go1 mvar ReceiveContribution     = processActions mvar
    go1 _ (Commit m)                 = do
        liftIO $ putLogLn configVerbosity 2
               $ printf "node %s commits to message %s"
                        (unparse myAddress)
                        (show m)
        
        s <- use committedScore
        previousScore .= s
        
        when (not configOmitMessageList) $ do
          committedMessages %= (|> m)
        
        i <- use scoreMultiplier
        committedScore += (fromIntegral i * m)
        scoreMultiplier += 1
    
    -- We'll spend most of our time here, waiting for actions from the helper threads. When they send
    -- us a contribution, we return them so 'go' and 'go1' can continue interpreting the Program, but
    -- control will return here soon enough.
    processActions :: MVar Action -> MaybeT M Contribution
    processActions mvar = (liftIO $ takeMVar mvar) >>= \case
        AddConnection remoteAddress connection -> do
          liftIO $ putLogLn configVerbosity 1
                 $ printf "node %s connected with %s"
                          (unparse myAddress)
                          (unparse remoteAddress)
          
          activeConnections %= Map.insert remoteAddress connection
          
          -- make sure that node is up to date
          c <- use latestContribution
          liftIO $ sendOne (myIndex, c) connection
          
          liftIO $ putLogLn configVerbosity 3
                 $ printf "node %s sends %s to %s"
                          (unparse myAddress)
                          (show c)
                          (unparse remoteAddress)
          
          processActions mvar
        RemoveConnection remoteAddress -> do
          liftIO $ putLogLn configVerbosity 1
                 $ printf "node %s lost connection with %s"
                          (unparse myAddress)
                          (unparse remoteAddress)
          
          activeConnections %= Map.delete remoteAddress
          processActions mvar
        ProcessContribution i c -> do
          let remoteAddress = indexToAddress i
          liftIO $ putLogLn configVerbosity 3
                 $ printf "node %s receives %s from %s"
                          (unparse myAddress)
                          (show c)
                          (unparse remoteAddress)
          
          -- ping-pong! send a message whenever we receive one
          (Map.lookup remoteAddress <$> use activeConnections) >>= \case
            Nothing ->
              return ()
            Just connection -> do
              c' <- use latestContribution
              liftIO $ sendOne (myIndex, c') connection
              
              liftIO $ putLogLn configVerbosity 3
                     $ printf "node %s sends %s to %s"
                              (unparse myAddress)
                              (show c')
                              (unparse remoteAddress)
          
          return c
        StopSendingNow -> do
          canSendContributions .= False
          processActions mvar
        PrintResultNow ->
          terminate
    
    terminate :: MaybeT M a
    terminate = do
        liftIO $ putLogLn configVerbosity 1 $ "final result:"
        
        if configOmitMessageList
        then do
          result <- use committedScore
          liftIO $ print result
        else do
          ms <- toList <$> use committedMessages
          s <- use committedScore
          liftIO $ print (ms, s)
        
        s <- use previousScore
        liftIO $ putLogLn configVerbosity 1
               $ printf "we don't guarantee that every node receives the last message,\nso other nodes might say %g" s
        
        -- abort the @MaybeT M a@ computation
        fail "the program has terminated"
    
    go :: MVar Action -> Program Void -> M ()
    go mvar = untilNothingM $ \case
        Return bottom -> absurd bottom
        Bind cr cc -> runMaybeT (cc <$> go1 mvar cr)
    
    timeKeeper :: MVar Action -> IO ()
    timeKeeper mvar = do
        let sendingDuration = timeIntervalToDiffTime configMessageSendingDuration
        let totalDuration   = timeIntervalToDiffTime (configMessageSendingDuration + configGracePeriodDuration)
        let terminationTime = totalDuration `addUTCTime` startTime
        
        -- if we wait until the end of the grace period we'll be killed before printing anything
        let printResultTime = timeIntervalToDiffTime (seconds (-2)) `addUTCTime` terminationTime
        
        -- if the grace period is really short, we might want to shorten the sending period as well
        let stopSendingTime = printResultTime `min` (sendingDuration `addUTCTime` startTime)
        
        sleepUntil stopSendingTime
        putMVar mvar StopSendingNow
        putLogLn configVerbosity 1 $ "no messages can be sent anymore."
        
        sleepUntil printResultTime
        putLogLn configVerbosity 1 $ "better print the output before it's too late."
        putMVar mvar PrintResultNow
    
    contributionReceiver :: MVar Action -> TransportT IO ()
    contributionReceiver mvar = receiveMany endpoint >>= \case
        Received labelledContributions -> do
          when (length labelledContributions > 1) $ do
            liftIO $ putLogLn configVerbosity 1
                   $ printf "node %s receives %d contributions at once, that's unusual"
                            (unparse myAddress)
                            (length labelledContributions)
          liftIO $ forM_ labelledContributions $ \(i,c) ->
            putMVar mvar (ProcessContribution i c)
          contributionReceiver mvar
        BrokenConnection remoteAddress -> do
          -- stop sending messages to that address until 'connectLater' tells us we're reconnected
          liftIO $ putMVar mvar $ RemoveConnection remoteAddress
          liftIO $ connectLater mvar remoteAddress
          contributionReceiver mvar
        ClosedConnection remoteAddress -> do
          -- this other node has terminated, stop sending it messages.
          liftIO $ putMVar mvar $ RemoveConnection remoteAddress
          
          -- the grace period will end soon, but let's keep waiting for messages from other nodes
          -- in an attempt to get a better score
          contributionReceiver mvar
        ClosedEndpoint ->
          -- the main thread has terminated, better stop too.
          return ()
    
    connectLater :: MVar Action -> Address -> IO ()
    connectLater mvar remoteAddress = void $ forkIO $ do
        -- 'createUnprotectedConnection' already tries to connect until it succeeds,
        -- so there is nothing special to do
        connection <- createUnprotectedConnection configVerbosity endpoint remoteAddress
        
        putMVar mvar $ AddConnection remoteAddress connection
