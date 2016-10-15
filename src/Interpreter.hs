{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Interpreter (interpret) where

import Control.Concurrent
import Control.Distributed.Process.Extras.Time
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Data.Void
import GHC.Generics
import Network.Transport
import System.Random

import Config hiding (Command)
import Control.Monad.MyExtra
import Data.Binary.Strict
import Log
import Message
import Network.Transport.MyExtra
import Network.Transport.TCP.Address
import Program


-- From the Program's point of view, only Contributions are being transmitted, but in order to stop
-- after the allocated time expires, we need other kinds of messages.
data Action
  = ProcessContribution Contribution
  | StopSendingNow
  deriving (Generic, Eq, Show)

instance Binary Action


data InterpreterState = InterpreterState
  { _pendingActions :: Maybe [Action]  -- received but not yet passed on to the Program.
                                       -- @Nothing@ means we should call 'receiveMany',
                                       -- @Just []@ means we should tell the Program it's the end of the list
  , _canSendContributions :: Bool
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState = InterpreterState
                        { _pendingActions       = Nothing
                        , _canSendContributions = True
                        }

makeLenses ''InterpreterState


-- the interpreter's monad transformer stack: its state, the random number's state, and IO
type M a = StateT InterpreterState (StateT StdGen IO) a

runM :: Int -> M a -> IO a
runM seed = flip evalStateT (mkStdGen seed)
          . flip evalStateT initialInterpreterState


interpret :: UserProvidedConfig -> Int -> NodeIndex -> Address -> EndPoint -> [Connection] -> Program Void -> IO ()
interpret (UserProvidedConfig {..}) nbNodes myIndex myAddress endpoint connections program = do
    _ <- forkIO timeKeeper
    runM mySeed (go program)
  where
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: Command a -> M a
    go1 (Log v s)                 = liftIO $ putLogLn configVerbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = lift randomMessage
    go1 (BroadcastContribution c) = use canSendContributions >>= \case
        True  -> liftIO $ mapM_ (sendOne (ProcessContribution c)) connections
        False -> return ()
    go1 ReceiveContribution       = use pendingActions >>= \case
        Nothing -> do
          -- we have not called 'receiveMany' yet, do it now
          cs <- liftIO $ receiveMany endpoint
          pendingActions .= Just cs
          go1 ReceiveContribution
        Just (ProcessContribution c:cs) -> do
          pendingActions .= Just cs
          return (Just c)
        Just (StopSendingNow:cs) -> do
          pendingActions .= Just cs
          canSendContributions .= False
          go1 ReceiveContribution
        Just [] -> do
          -- reset to 'Nothing' so the next call blocks with 'receiveMany' again, and
          -- tell 'receiveContributions' that the list is over
          pendingActions .= Nothing
          return Nothing
    go1 (Commit _)                = return ()
    
    go :: Program Void -> M ()
    go = untilNothingM $ \case
        Return void -> absurd void
        Bind cr cc -> do
          r <- go1 cr
          return $ Just (cc r)
    
    timeKeeper :: IO ()
    timeKeeper = do
        selfConnection <- connectStubbornly endpoint myAddress
        threadDelay $ asTimeout configMessageSendingDuration
        sendOne StopSendingNow selfConnection
        putLogLn configVerbosity 1 $ "no messages can be sent anymore."
