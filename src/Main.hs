{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Concurrent (threadDelay)
import           Control.Monad
import qualified Data.ByteString.Char8 as ByteString
import           Data.String
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           Options.Applicative (execParser)
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Config (Command(..), commandInfo, FileProvidedConfig(..), Role(..), UserProvidedConfig(..))


fromRightM :: (Show e, Monad m) => Either e a -> m a
fromRightM (Left  e) = fail $ show e
fromRightM (Right x) = return x

untilM :: Monad m => m (Maybe a) -> m a
untilM body = body >>= \case
    Nothing -> untilM body
    Just x  -> return x


createTransportStubbornly :: String -> Int -> IO Transport
createTransportStubbornly host port = untilM $ do
    r <- createTransport host (show port) defaultTCPParameters
    case r of
      Left err | isAlreadyInUseError err -> do
        -- sometimes the OS keeps sockets busy for a minute after a server stops, try again
        putStrLn "local port is busy, retrying..."
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left err ->
        fail $ show err
      Right transport ->
        return $ Just transport

connectStubbornly :: EndPoint -> String -> Int -> IO Connection
connectStubbornly endpoint host port = untilM $ do
    r <- connect endpoint address ReliableOrdered defaultConnectHints
    case r of
      Left (TransportError ConnectNotFound _) -> do
        -- the remote program probably isn't fully-initialized yet, try again.
        putStrLn "remote unreachable, retrying..."
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left (TransportError _ err) ->
        fail err
      Right connection ->
        return $ Just connection
  where
    address :: EndPointAddress
    address = EndPointAddress $ ByteString.pack $ printf "%s:%d:0" host port


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
