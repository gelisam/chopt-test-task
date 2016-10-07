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


server :: IO ()
server = do
    transport <- createTransportStubbornly "127.0.0.1" 10080
    endpoint <- fromRightM =<< newEndPoint transport
    
    forever $ do
      event <- receive endpoint
      case event of
        Received _ msg -> print msg
        _ -> return () -- ignore

client :: IO ()
client = do
    transport <- createTransportStubbornly "127.0.0.1" 10081
    endpoint <- fromRightM =<< newEndPoint transport
    
    conn <- connectStubbornly endpoint "127.0.0.1" 10080
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
              (FileProvidedConfig role
                                  host port)
              -> do
        case role of
          Master -> server
          Slave  -> client
