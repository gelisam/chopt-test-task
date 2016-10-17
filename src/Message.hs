{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE RecordWildCards #-}
module Message where

import           Prelude hiding (round)

import           Control.Monad.Trans.State.Strict
import           Data.Bits
import           GHC.Generics
import           System.Random

import           Data.Binary.Strict


type Message = Double

randomMessage :: Monad m => StateT StdGen m Message
randomMessage = state $ randomR (0, 1)


type RoundNumber = Int
type NodeIndex = Int
type BitSet = Integer 


-- In each round, we accumulate the candidate messages from all the nodes, we keep the
-- best one, and we commit to it. Since nodes won't contribute to a round until they have
-- all the information from the previous one, we're guaranteed not to commit to message
-- n+1 until all the other nodes have committed to message n or above. Since all the nodes
-- do this, we are guaranteed that once the exercise terminates, all the nodes will output
-- messages 0 through n, some nodes will also output message n+1, but no node output will
-- differ by more than this one message.
data Contribution = Contribution
  { currentRoundNumber   :: !RoundNumber
  , previousMessage      :: !Message
  , currentBestCandidate :: !Message
  , roundContributors    :: !BitSet
  }
  deriving (Eq, Generic, Show)

instance Binary Contribution

instance Monoid Contribution where
    mempty = Contribution 0 0 0 0
    
    -- this mappend is also commutative and idempotent
    mappend s1@(Contribution r1 p1 m1 c1) s2@(Contribution r2 p2 m2 c2)
      | r1 > r2 = s1
      | r2 > r1 = s2
      | otherwise = Contribution
                  { currentRoundNumber   = if r1 == r2 then r1 else error "inconsistent round numbers"
                  , previousMessage      = if p1 == p2 then p1 else error "inconsistent previous messages"
                  , currentBestCandidate = max m1 m2  -- if we're going to drop messages, let's keep
                                                      -- the one with the highest score :)
                  , roundContributors    = c1 .|. c2
                  }

firstRound :: NodeIndex -> Message -> Contribution
firstRound myIndex message = Contribution
                           { currentRoundNumber   = 0
                           , previousMessage      = 0  -- a dummy value, there is no round -1
                           , currentBestCandidate = message
                           , roundContributors    = bit myIndex  -- we only know about our own contribution
                           }

nextRound :: NodeIndex -> Message -> Contribution -> Contribution
nextRound myIndex message (Contribution {..}) = Contribution
                                              { currentRoundNumber   = currentRoundNumber + 1
                                              , previousMessage      = currentBestCandidate
                                              , currentBestCandidate = message
                                              , roundContributors    = bit myIndex
                                              }

countRoundContributors :: Contribution -> Int
countRoundContributors = popCount . roundContributors
