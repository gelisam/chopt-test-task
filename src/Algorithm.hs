{-# LANGUAGE BangPatterns #-}
module Algorithm where

import Prelude hiding (log, round)

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Text.Printf

import Program
import Message


-- runs forever
algorithm :: Program void
algorithm = do
    status0 <- initialOverallStatus <$> getNbNodes
                                    <*> getMyNodeIndex
                                    <*> generateRandomMessage
                                    <*> generateRandomMessage
    
    flip evalStateT status0 $ forever $ do
      message <- runRound
      lift $ commit message
      
      myIndex <- lift getMyNodeIndex
      roundNumber <- currentRoundNumber <$> get
      lift $ log 2 $ printf "node #%d agrees: round %d's message is %s"
                            myIndex
                            roundNumber
                            (show message)
      
      nextCandidate <- lift generateRandomMessage
      modify $ moveToNextRound nextCandidate

runRound :: StateT OverallStatus Program Message
runRound = do
    -- at this point, we have already chosen a message. share it with everywone.
    myContribution <- currentContribution <$> get
    lift $ broadcastContribution myContribution
    
    myIndex <- lift getMyNodeIndex
    lift $ log 4 $ printf "node #%d sends %s" myIndex (show myContribution)
    
    -- collect the other contributions and return the best one
    loop
  where
    loop :: StateT OverallStatus Program Message
    loop = do
        isComplete <- isCurrentRoundComplete <$> get
        if isComplete
        then
          bestMessageForCurrentRound <$> get
        else do
          contributions <- lift receiveContributions
          mapM_ (modify . collectContribution) contributions
          
          myIndex <- lift getMyNodeIndex
          forM_ contributions $ \contribution ->
            lift $ log 4 $ printf "node #%d received %s" myIndex (show contribution)
          newStatus <- get
          lift $ log 3 $ printf "node #%d now at %s" myIndex (show newStatus)
          
          loop
