{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.String
import           Network.Transport
import           Options.Applicative (execParser)
import           Text.Printf

import           Config (Command(..), commandInfo, FileProvidedConfig(..), UserProvidedConfig(..))
import           Control.Monad.MyExtra
import           Network.Transport.MyExtra
import           Network.Transport.TCP.Address
import           Text.Parsable


runNode :: Address -> [Address] -> IO ()
runNode myAddress peerAddresses = do
    endpoint <- createEndpointStubbornly myAddress
    connections <- mapM (connectStubbornly endpoint) peerAddresses
    
    -- send a message to everyone
    let myMessage = fromString $ printf "hello from %s" (unparse myAddress)
    forM_ connections $ \connection ->
      join $ fromRightM <$> send connection [myMessage]
    
    -- receive a message from everyone
    untilTotalM (length connections) $ receive endpoint >>= \case
      Received _ messages -> do
        forM_ messages $ \message ->
          printf "%s received %s\n" (unparse myAddress) (show message)
        return (length messages)
      ConnectionOpened {} ->
        -- ignored
        return 0
      err -> do
        fail (show err)

main :: IO ()
main = do
    command <- execParser commandInfo
    case command of
      CheckArgs _ ->
        -- if the arguments were incorrect, 'execParser' would have complained.
        return ()
      RunNode (UserProvidedConfig messageSendingDuration gracePeriodDuration
                                  randomSeed)
              (FileProvidedConfig myAddress)
              -> do
        allAddresses <- join $ mapM parse <$> lines <$> readFile "nodelist.txt"
        let peerAddresses = filter (/= myAddress) allAddresses
        runNode myAddress peerAddresses
