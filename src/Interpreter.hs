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
-- This reconnection thread repeatedly attempts to reconnect, and doesn't need to inform anyone when
-- it succeeds, because 'receiveMany' already receives a notification when a new connection is
-- established.

{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Interpreter (interpret) where

import           Control.Concurrent
import           Control.Distributed.Process.Extras.Time
import           Control.Lens (makeLenses, use, (.=), (%=), (+=))
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.State.Strict
import           Data.Foldable
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
  = ProcessContributions [Contribution]
  | StopSendingNow
  | PrintResultNow


data InterpreterState = InterpreterState
  { _canSendContributions :: !Bool
  , _committedMessages    :: !(Seq Message)
  , _previousScore        :: !Double
  , _committedScore       :: !Double
  }

initialInterpreterState :: InterpreterState
initialInterpreterState = InterpreterState
                        { _canSendContributions = True
                        , _committedMessages    = mempty
                        , _previousScore        = 0
                        , _committedScore       = 0
                        }

makeLenses ''InterpreterState


-- the interpreter's monad transformer stack: its state, the random number's state, and IO
type M = StateT InterpreterState (StateT StdGen IO)

runM :: Int -> M a -> IO a
runM seed = flip evalStateT (mkStdGen seed)
          . flip evalStateT initialInterpreterState


interpret :: UserProvidedConfig -> UTCTime -> Int -> NodeIndex -> Address -> Endpoint -> [Connection] -> Program Void -> IO ()
interpret (UserProvidedConfig {..}) startTime nbNodes myIndex myAddress endpoint connections program = do
    mvar <- newEmptyMVar
    _ <- forkIO $ timeKeeper mvar
    _ <- forkIO $ runTransportT $ contributionReceiver mvar
    runM mySeed (go mvar program)
  where
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: MVar Action -> Command a -> MaybeT M a
    go1 _ (Log v s)                 = liftIO $ putLogLn configVerbosity v s
    go1 _ GetNbNodes                = return nbNodes
    go1 _ GetMyNodeIndex            = return myIndex
    go1 _ GenerateRandomMessage     = lift . lift $ randomMessage
    go1 _ (BroadcastContribution c) = use canSendContributions >>= \case
        True  -> liftIO $ mapM_ (sendOne c) connections
        False -> return ()
    go1 mvar ReceiveContributions   = processActions mvar
    go1 _ (Commit m)                = do
        s <- use committedScore
        previousScore .= s
        
        committedMessages %= (|> m)
        i <- length <$> use committedMessages
        committedScore += (fromIntegral i * m)
    
    -- We'll spend most of our time here, waiting for actions from the helper threads. When they send
    -- us a list of contributions, we return them so 'go' and 'go1' can continue interpreting the
    -- Program, but control will return here soon enough.
    processActions :: MVar Action -> MaybeT M [Contribution]
    processActions mvar = (liftIO $ takeMVar mvar) >>= \case
        ProcessContributions cs ->
          return cs
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
        Return void -> absurd void
        Bind cr cc -> runMaybeT (cc <$> go1 mvar cr)
    
    timeKeeper :: MVar Action -> IO ()
    timeKeeper mvar = do
        let sendingDuration = timeIntervalToDiffTime configMessageSendingDuration
        let totalDuration   = timeIntervalToDiffTime (configMessageSendingDuration + configGracePeriodDuration)
        let terminationTime = totalDuration `addUTCTime` startTime
        
        -- if we wait until the end of the grace period we'll be killed before printing anything
        let printResultTime = timeIntervalToDiffTime (seconds (-1)) `addUTCTime` terminationTime
        
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
        Received cs -> do
          liftIO $ putMVar mvar (ProcessContributions cs)
          contributionReceiver mvar
        BrokenConnection remoteAddress ->
          -- we don't support broken connections yet
          fail $ printf "node %s lost connection with %s"
                        (unparse myAddress)
                        (unparse remoteAddress)
        ClosedConnection _ ->
          -- another node has terminated, stop listening for more contributions.
          -- TODO: wait for messages from other nodes in an attempt to get a better score
          return ()
        ClosedEndpoint ->
          -- the main thread has terminated, better stop too.
          return ()
