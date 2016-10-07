{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad
import           Data.String
import           Network.Transport
import           Options.Applicative (execParser)

import           Control.Monad.MyExtra
import           Config (Command(..), commandInfo, FileProvidedConfig(..), Role(..), UserProvidedConfig(..))
import           Network.Transport.MyExtra
import           Network.Transport.TCP.Address


server :: Address -> IO ()
server myAddress = do
    transport <- createTransportStubbornly myAddress
    endpoint <- join $ fromRightM <$> newEndPoint transport
    
    forever $ do
      event <- receive endpoint
      case event of
        Received _ msg -> print msg
        _ -> return () -- ignore

client :: Address -> Address -> IO ()
client myAddress serverAddress = do
    transport <- createTransportStubbornly myAddress
    endpoint <- join $ fromRightM <$> newEndPoint transport
    
    conn <- connectStubbornly endpoint serverAddress
    _ <- send conn [fromString "Hello world"]
    return ()

main :: IO ()
main = do
    command <- execParser commandInfo
    case command of
      CheckArgs _ ->
        -- if the arguments were incorrect, 'execParser' would have complained.
        return ()
      RunNode (UserProvidedConfig messageSendingDuration gracePeriodDuration
                                  randomSeed)
              (FileProvidedConfig role myAddress)
              -> do
        case role of
          Master -> join $ server <$> parseAddress myAddress
          Slave  -> join $ client <$> parseAddress myAddress <*> parseAddress "localhost:8081:0"
