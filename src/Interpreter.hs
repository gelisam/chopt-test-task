{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Interpreter (interpret) where

import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Network.Transport
import System.Random

import Config hiding (Command)
import Log
import Message
import Network.Transport.MyExtra
import Program


data InterpreterState = InterpreterState
  { _pendingContributions :: Maybe [Contribution]  -- received but not yet passed on to the Program.
                                                   -- @Nothing@ means we should call 'receiveMany',
                                                   -- @Just []@ means we should tell the Program it's the end of the list
  , _canSendContributions :: Bool
  }
  deriving (Eq, Show)

initialInterpreterState :: InterpreterState
initialInterpreterState = InterpreterState
                        { _pendingContributions = Nothing
                        , _canSendContributions = True
                        }

makeLenses ''InterpreterState


interpret :: UserProvidedConfig -> Int -> NodeIndex -> EndPoint -> [Connection] -> Program a -> IO a
interpret (UserProvidedConfig {..}) nbNodes myIndex endpoint connections
  = flip evalStateT (mkStdGen mySeed)
  . flip evalStateT initialInterpreterState
  . go
  where
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: Command a -> StateT InterpreterState (StateT StdGen IO) a
    go1 (Log v s)                 = liftIO $ putLogLn configVerbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = lift randomMessage
    go1 (BroadcastContribution c) = use canSendContributions >>= \case
        True  -> liftIO $ mapM_ (sendOne c) connections
        False -> return ()
    go1 ReceiveContribution       = use pendingContributions >>= \case
        Nothing -> do
          -- we have not called 'receiveMany' yet, do it now
          cs <- liftIO $ receiveMany endpoint
          pendingContributions .= Just cs
          go1 ReceiveContribution
          -- cs is non-empty, "fall through" to the next line
        Just (c:cs) -> do
          pendingContributions .= Just cs
          return (Just c)
        Just [] -> do
          -- reset to 'Nothing' so the next call blocks with 'receiveMany' again, and
          -- tell 'receiveContributions' that the list is over
          pendingContributions .= Nothing
          return Nothing
    go1 (Commit _)                = return ()
    
    go :: Program a -> StateT InterpreterState (StateT StdGen IO) a
    go (Return x) = return x
    go (Bind cr cc) = do
      r <- go1 cr
      go (cc r)
