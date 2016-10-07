{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra where

import           Control.Concurrent (threadDelay)
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.IO.Error (isAlreadyInUseError)

import           Control.Monad.MyExtra
import           Network.Transport.TCP.Address


createTransportStubbornly :: Address -> IO Transport
createTransportStubbornly (Address {..}) = untilM $ do
    r <- createTransport addressHost (show addressPort) defaultTCPParameters
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

connectStubbornly :: EndPoint -> Address -> IO Connection
connectStubbornly localEndpoint remoteAddress = untilM $ do
    r <- connect localEndpoint (endpointAddress remoteAddress) ReliableOrdered defaultConnectHints
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
