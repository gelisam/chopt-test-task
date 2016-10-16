{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Interpreter (interpret) where

import Control.Concurrent
import Control.Distributed.Process.Extras.Time
import Control.Lens (makeLenses, use, (.=), (%=), (+=))
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Resource
import Control.Monad.Trans.State.Strict
import Data.Foldable
import Data.Sequence (Seq, (|>))
import Data.Time
import Data.Void
import GHC.Generics
import System.Random
import Text.Printf

import Config hiding (Command)
import Control.Concurrent.MyExtra
import Control.Monad.MyExtra
import Data.Binary.Strict
import Log
import Message
import Network.Transport.MyExtra
import Network.Transport.TCP.Address
import Program
import Text.Parsable


-- From the Program's point of view, only Contributions are being transmitted, but in order to stop
-- after the allocated time expires, we need other kinds of messages.
data Action
  = ProcessContribution Contribution
  | StopSendingNow
  | PrintResultNow
  deriving (Generic, Eq, Show)

instance Binary Action


data InterpreterState = InterpreterState
  { _pendingActions       :: Maybe [Action]  -- received but not yet passed on to the Program.
                                             -- @Nothing@ means we should call 'receiveMany',
                                             -- @Just []@ means we should tell the Program it's the end of the list
  , _canSendContributions :: !Bool
  , _committedMessages    :: !(Seq Message)
  , _previousScore        :: !Double
  , _committedScore       :: !Double
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState = InterpreterState
                        { _pendingActions       = Nothing
                        , _canSendContributions = True
                        , _committedMessages    = mempty
                        , _previousScore        = 0
                        , _committedScore       = 0
                        }

makeLenses ''InterpreterState


-- the interpreter's monad transformer stack: its state, the random number's state, and IO
type M = StateT InterpreterState (StateT StdGen (TransportT IO))

runM :: Int -> M a -> IO a
runM seed = runTransportT
          . flip evalStateT (mkStdGen seed)
          . flip evalStateT initialInterpreterState


interpret :: UserProvidedConfig -> UTCTime -> Int -> NodeIndex -> Address -> EndPoint -> [Connection] -> Program Void -> IO ()
interpret (UserProvidedConfig {..}) startTime nbNodes myIndex myAddress endpoint connections program = do
    _ <- forkIO timeKeeper
    runM mySeed (go program)
  where
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: Command a -> MaybeT M a
    go1 (Log v s)                 = liftIO $ putLogLn configVerbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = lift . lift $ randomMessage
    go1 (BroadcastContribution c) = use canSendContributions >>= \case
        True  -> liftIO $ mapM_ (sendOne (ProcessContribution c)) connections
        False -> return ()
    go1 ReceiveContribution       = waitForContribution
    go1 (Commit m)                = do
        s <- use committedScore
        previousScore .= s
        
        committedMessages %= (|> m)
        i <- length <$> use committedMessages
        committedScore += (fromIntegral i * m)
    
    waitForContribution :: MaybeT M (Maybe Contribution)
    waitForContribution = use pendingActions >>= \case
        Nothing -> do
          -- we have not called 'receiveMany' yet, do it now
          r <- lift . lift . lift $ receiveMany endpoint
          case r of
            Received cs -> do
              pendingActions .= Just cs
              waitForContribution
            ClosedConnection _ ->
              -- another node has terminated, terminate too.
              -- TODO: wait for messages from other nodes in an attempt to get a better score
              terminate
            BrokenConnection remoteAddress ->
              -- we don't support broken connections yet
              fail $ printf "node %s lost connection with %s"
                            (unparse myAddress)
                            (unparse remoteAddress)
        Just (ProcessContribution c:cs) -> do
          pendingActions .= Just cs
          return (Just c)
        Just (StopSendingNow:cs) -> do
          pendingActions .= Just cs
          canSendContributions .= False
          waitForContribution
        Just (PrintResultNow:_) -> terminate
        Just [] -> do
          -- reset to 'Nothing' so the next call blocks with 'receiveMany' again, and
          -- tell 'receiveContributions' that the list is over
          pendingActions .= Nothing
          return Nothing
    
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
    
    go :: Program Void -> M ()
    go = untilNothingM $ \case
        Return void -> absurd void
        Bind cr cc -> runMaybeT (cc <$> go1 cr)
    
    timeKeeper :: IO ()
    timeKeeper = runResourceT $ do
        let sendingDuration = timeIntervalToDiffTime configMessageSendingDuration
        let totalDuration   = timeIntervalToDiffTime (configMessageSendingDuration + configGracePeriodDuration)
        let terminationTime = totalDuration `addUTCTime` startTime
        
        -- if we wait until the end of the grace period we'll be killed before printing anything
        let printResultTime = timeIntervalToDiffTime (seconds (-1)) `addUTCTime` terminationTime
        
        -- if the grace period is really short, we might want to shorten the sending period as well
        let stopSendingTime = printResultTime `min` (sendingDuration `addUTCTime` startTime)
        
        selfConnection <- snd <$> createConnection endpoint myAddress
        
        liftIO $ do
          sleepUntil stopSendingTime
          sendOne StopSendingNow selfConnection
          putLogLn configVerbosity 1 $ "no messages can be sent anymore."
          
          sleepUntil printResultTime
          putLogLn configVerbosity 1 $ "better print the output before it's too late."
          sendOne PrintResultNow selfConnection
