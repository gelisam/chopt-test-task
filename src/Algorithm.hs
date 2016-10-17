{-# LANGUAGE BangPatterns #-}
module Algorithm where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State.Strict
import           Data.Monoid
import           Data.Void

import           Program
import           Message


-- runs forever
algorithm :: Program Void
algorithm = do
    contribution0 <- firstRound <$> getMyNodeIndex
                                <*> generateRandomMessage
    nbNodes <- getNbNodes
    
    flip evalStateT contribution0 $ forever $ do
      -- share our contibution
      c <- get
      lift $ broadcastContribution c
      
      -- receive the contributions shared by others
      cs <- lift $ receiveContributions
      modify (<> mconcat (c:cs))
      c' <- get
      
      -- determine if we should commit a message
      when (currentRoundNumber c' > currentRoundNumber c) $ do
        when (currentRoundNumber c' /= currentRoundNumber c + 1) $ do
          -- should not happen because at most two rounds should be active at any time
          fail "inconsistent difference in round numbers"
        
        let m = previousMessage c'
        lift $ commit m
        
        -- the next round has already started and we haven't contributed a message yet!
        candidate <- nextRound <$> lift getMyNodeIndex
                               <*> lift generateRandomMessage
                               <*> pure (c { currentBestCandidate = m })
        modify (<> candidate)
      c'' <- get
      
      -- determine if we should commit another message
      when (countRoundContributors c'' == nbNodes) $ do
        lift $ commit $ currentBestCandidate c''
        candidate <- nextRound <$> lift getMyNodeIndex
                               <*> lift generateRandomMessage
                               <*> pure c''
        modify (<> candidate)
