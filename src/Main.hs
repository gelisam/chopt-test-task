{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad
import Data.String
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Options.Applicative (execParser)

import Config (Command(..), commandInfo, FileProvidedConfig(..), Role(..), UserProvidedConfig(..))


fromRightM :: (Show e, Monad m) => Either e a -> m a
fromRightM (Left  e) = fail (show e)
fromRightM (Right x) = return x


server :: IO ()
server = do
    transport <- fromRightM =<< createTransport "127.0.0.1" "10080" defaultTCPParameters
    endpoint <- fromRightM =<< newEndPoint transport
    
    forever $ do
      event <- receive endpoint
      case event of
        Received _ msg -> print msg
        _ -> return () -- ignore

client :: IO ()
client = do
    transport <- fromRightM =<< createTransport "127.0.0.1" "10081" defaultTCPParameters
    endpoint <- fromRightM =<< newEndPoint transport
    
    let serverAddr = EndPointAddress "127.0.0.1:10080:0"
    conn <- fromRightM =<< connect endpoint serverAddr ReliableOrdered defaultConnectHints
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
