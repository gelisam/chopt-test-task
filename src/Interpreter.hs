{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Interpreter where

import Control.Monad.IO.Class
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
  = flip evalStateT (mkStdGen mySeed)
  . flip evalStateT Nothing
  . go
  where
    -- combine the shared configRandomSeed with the node index so that each node uses a different
    -- sequence of messages
    mySeed :: Int
    mySeed = configRandomSeed + myIndex
    
    go1 :: Command a -> StateT (Maybe [Contribution]) (StateT StdGen IO) a
    go1 (Log v s)                 = liftIO $ putLogLn configVerbosity v s
    go1 GetNbNodes                = return nbNodes
    go1 GetMyNodeIndex            = return myIndex
    go1 GenerateRandomMessage     = lift randomMessage
    go1 (BroadcastContribution c) = liftIO $ mapM_ (sendOne c) connections
    go1 ReceiveContribution       = get >>= \case
        Nothing -> do
          -- we have not called 'receiveMany' yet, do it now
          cs <- liftIO $ receiveMany endpoint
          put $ Just cs
          go1 ReceiveContribution
          -- cs is non-empty, "fall through" to the next line
        Just (c:cs) -> do
          put (Just cs)
          return (Just c)
        Just [] -> do
          -- reset to 'Nothing' so the next call blocks with 'receiveMany' again, and
          -- tell 'receiveContributions' that the list is over
          put Nothing
          return Nothing
    go1 (Commit _)                = return ()
    
    go :: Program a -> StateT (Maybe [Contribution]) (StateT StdGen IO) a
    go (Return x) = return x
    go (Bind cr cc) = do
      r <- go1 cr
      go (cc r)
