{-# LANGUAGE BangPatterns #-}
module Algorithm where

import Prelude hiding (round)

import Control.Monad
import Data.Bits
import Text.Printf

import Program
import Message


-- runs forever
algorithm :: Program ()
algorithm = do
    forM_ [0..] $ \roundNumber -> do
      message <- runRound roundNumber
      
      myIndex <- getMyNodeIndex
      debug $ printf "node #%d agrees: round %d's message is %s\n" myIndex roundNumber (show message)

-- the round is complete when the information is accumulated, that is, once we've
-- combined the candidate messages from all the contributors.
isRoundComplete :: RoundStatus -> Program Bool
isRoundComplete status = do
    nbNodes <- getNbNodes
    return $ countRoundContributors status == nbNodes

runRound :: RoundNumber -> Program Message
runRound currentRoundNumber = do
    -- choose a candidate and send this contribution to everyone
    myMessage <- command GenerateRandomMessage
    myIndex <- getMyNodeIndex
    let myStatus = RoundStatus myMessage (bit myIndex)
    let myContribution = (currentRoundNumber, myStatus)
    command $ BroadcastContribution myContribution
    
    -- collect the other contributions and return the best one
    go myStatus
  where
    go :: RoundStatus -> Program Message
    go !status = do
        isComplete <- isRoundComplete status
        if isComplete
        then
          return $ roundMessage status
        else do
          statuses <- map snd <$> filter current <$> receiveContributions
          let newStatus = mconcat (status:statuses)
          
          myIndex <- getMyNodeIndex
          debug $ printf "node #%d %s => %s\n" myIndex (show status) (show newStatus)
          
          go newStatus
    
    current :: Contribution -> Bool
    current (roundNumber, _) = roundNumber == currentRoundNumber
