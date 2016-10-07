module Network.Transport.MyExtra where

import           Control.Concurrent (threadDelay)
import qualified Data.ByteString.Char8 as ByteString
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Control.Monad.MyExtra


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
