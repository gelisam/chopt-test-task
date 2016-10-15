{-# LANGUAGE GADTs #-}
{-# LANGUAGE RecordWildCards #-}
module Interpreter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.State
import Network.Transport
import System.Random

import Config hiding (Command)
import Log
import Message
import Network.Transport.MyExtra
import Program


interpret :: UserProvidedConfig -> Int -> NodeIndex -> EndPoint -> [Connection] -> Program a -> IO a
interpret (UserProvidedConfig {..}) nbNodes myIndex endpoint connections
  = flip evalStateT (mkStdGen configRandomSeed) . go
  where
    go1 :: Command a -> StateT StdGen IO a
    go1 (Log v s)                 = lift $ putLogLn configVerbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = randomMessage
    go1 (BroadcastContribution c) = lift $ mapM_ (sendOne c) connections
    go1 ReceiveContributions      = lift $ receiveMany endpoint
    go1 (Commit _)                = return ()
    
    go :: Program a -> StateT StdGen IO a
    go (Return x) = return x
    go (Bind cr cc) = do
      r <- go1 cr
      go (cc r)
