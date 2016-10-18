{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- I'd like to simulate my algorithm in a pure interpreter who can simulate dropped
-- messages easily, and I also need to run the same algorithm in distributed-process.
-- This is a perfect opportunity to use the free monad :)
module Program where

import           Control.Monad (ap)

import           Message


-- I'm not using Control.Monad.Free because I want Commands to be indexed by their output
-- type, not by the output type of a post-computation we stick after every command just
-- so we can implement Functor.
data Command a where
    GetNbNodes :: Command Int
    GetMyNodeIndex :: Command NodeIndex  -- between 0 and NbNodes-1
    GenerateRandomMessage :: Command Message
    BroadcastContribution :: Contribution -> Command ()
    ReceiveContribution :: Command Contribution
    Commit :: Message -> Command ()


data Program a where
    Return :: a -> Program a
    Bind :: Command r -> (r -> Program a) -> Program a

instance Functor Program where
    fmap f (Return x) = Return (f x)
    fmap f (Bind cr cc) = Bind cr (fmap (fmap f) cc)

instance Applicative Program where
    pure = Return
    (<*>) = ap

instance Monad Program where
    Return x   >>= f = f x
    Bind cr cc >>= f = Bind cr (fmap (>>= f) cc)


liftCommand :: Command a -> Program a
liftCommand cmd = Bind cmd Return

getNbNodes :: Program Int
getNbNodes = liftCommand GetNbNodes

getMyNodeIndex :: Program NodeIndex
getMyNodeIndex = liftCommand GetMyNodeIndex

generateRandomMessage :: Program Message
generateRandomMessage = liftCommand GenerateRandomMessage

broadcastContribution :: Contribution -> Program ()
broadcastContribution contributions = liftCommand $ BroadcastContribution contributions

receiveContribution :: Program Contribution
receiveContribution = liftCommand ReceiveContribution

commit :: Message -> Program ()
commit message = liftCommand $ Commit message
