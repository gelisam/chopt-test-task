{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
module Message where

import Prelude hiding (round)

import Data.Bits
import GHC.Generics
import Network.Transport
import System.Random
import Text.Printf

import Data.Binary.Strict
import Network.Transport.MyExtra


type Message = Double

randomMessage :: IO Message
randomMessage = randomRIO (0, 1)


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

countRoundContributors :: RoundStatus -> Int
countRoundContributors = popCount . roundContributors
