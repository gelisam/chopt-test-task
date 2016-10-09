{-# LANGUAGE GADTs #-}
module Interpreter where

import Network.Transport

import Log
import Message
import Network.Transport.MyExtra
import Program


interpret :: Verbosity -> Int -> Int -> EndPoint -> [Connection] -> Program a -> IO a
interpret verbosity nbNodes myIndex endpoint connections = go
  where
    go1 :: Command a -> IO a
    go1 (Log v s)                 = putLogLn verbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = randomMessage
    go1 (BroadcastContribution c) = mapM_ (sendOne c) connections
    go1 ReceiveContributions      = receiveMany endpoint
    go1 (Commit _)                = return ()
    
    go :: Program a -> IO a
    go (Return x) = return x
    go (Bind cr cc) = do
      r <- go1 cr
      go (cc r)
