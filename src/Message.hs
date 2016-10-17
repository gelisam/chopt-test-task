{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Message where

import           Prelude hiding (round)

import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           Data.Monoid
import           Data.Sequence
import           GHC.Generics
import           System.Random
import           Text.Printf

import           Data.Binary.Strict


type Message = Double

randomMessage :: Monad m => StateT StdGen m Message
randomMessage = state $ randomR (0, 1)


type NodeIndex = Int
type BitSet = Integer 


-- In each round, we accumulate the candidate messages from all the nodes, we keep the
-- best one, and we commit to it. Since nodes won't contribute to a round until they have
-- all the information from the previous one, we're guaranteed not to commit to message
-- n+1 until all the other nodes have committed to message n or above. Since all the nodes
-- do this, we are guaranteed that once the exercise terminates, all the nodes will output
-- messages 0 through n, some nodes will also output message n+1, but no node output will
-- differ by more than this one message.
data RoundStatus = RoundStatus
  { roundMessage      :: !Message
  , roundContributors :: !BitSet
  }
  deriving (Eq, Generic, Show)

instance Binary RoundStatus

instance Monoid RoundStatus where
    mempty = RoundStatus 0 0
    
    -- this mappend is also commutative and idempotent
    mappend (RoundStatus m1 c1) (RoundStatus m2 c2) = RoundStatus bestMessage allContributors
      where
        -- if we're going to drop messages, let's keep the one with the highest score :)
        bestMessage = max m1 m2
        
        allContributors = c1 .|. c2

initialRoundStatus :: NodeIndex -> Message -> RoundStatus
initialRoundStatus myIndex message
  = RoundStatus { roundMessage      = message
                , roundContributors = bit myIndex  -- we only know about our own contribution
                }

countRoundContributors :: RoundStatus -> Int
countRoundContributors = popCount . roundContributors


type RoundNumber = Int

data OverallStatus = OverallStatus
  { numberOfNodes      :: !Int
  , indexOfCurrentNode :: !NodeIndex  -- between 0 and numberOfNodes-1
  , previousRounds     :: !(Seq Message)
  , currentRoundNumber :: !RoundNumber
  , currentRoundStatus :: !RoundStatus
  , nextRoundStatus    :: !RoundStatus
  }
  deriving (Eq, Show)

initialOverallStatus :: Int -> NodeIndex -> Message -> Message -> OverallStatus
initialOverallStatus nbNodes myIndex message1 message2
  = OverallStatus { numberOfNodes      = nbNodes
                  , indexOfCurrentNode = myIndex
                  , previousRounds     = mempty
                  , currentRoundNumber = 0
                  , currentRoundStatus = initialRoundStatus myIndex message1
                  , nextRoundStatus    = initialRoundStatus myIndex message2
                  }

-- the round is complete when all the information is accumulated, that is, once we've
-- combined the candidate messages from all the contributors.
isCurrentRoundComplete :: OverallStatus -> Bool
isCurrentRoundComplete (OverallStatus {..})
  = countRoundContributors currentRoundStatus == numberOfNodes

bestMessageForCurrentRound :: OverallStatus -> Message
bestMessageForCurrentRound = roundMessage . currentRoundStatus

moveToNextRound :: Message -> OverallStatus -> OverallStatus
moveToNextRound nextCandidate o@(OverallStatus {..})
  = o { previousRounds     = previousRounds |> bestMessageForCurrentRound o
      , currentRoundNumber = currentRoundNumber + 1
      , currentRoundStatus = nextRoundStatus
      , nextRoundStatus    = initialRoundStatus indexOfCurrentNode nextCandidate
      }


type Contribution = (RoundNumber, RoundStatus)

currentContribution :: OverallStatus -> Contribution
currentContribution (OverallStatus {..}) = (currentRoundNumber, currentRoundStatus)

collectContribution :: Contribution -> OverallStatus -> OverallStatus
collectContribution c@(roundNumber, status) o@(OverallStatus {..})
  | roundNumber == currentRoundNumber     = o { currentRoundStatus = currentRoundStatus <> status }
  | roundNumber == currentRoundNumber + 1 = o { nextRoundStatus    = nextRoundStatus    <> status }
  | roundNumber >  currentRoundNumber + 1 =
      error $ printf "the algorithm is broken! contribution %s should never have been sent since round %d is not yet decided"
                     (show c)
                     currentRoundNumber
  | otherwise =
      o  -- ignore contributions about already-decided rounds
