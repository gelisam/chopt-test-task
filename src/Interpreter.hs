{-# LANGUAGE GADTs #-}
module Interpreter where

import Prelude hiding (round)

import Network.Transport
import System.Random
import Text.Printf

import Data.Binary.Strict
import Message
import Network.Transport.MyExtra
import Program


interpret :: Int -> Int -> EndPoint -> [Connection] -> Program a -> IO a
interpret nbNodes myIndex endpoint connections = go
  where
    go1 :: Command a -> IO a
    go1 (Debug s)                 = printf s
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
