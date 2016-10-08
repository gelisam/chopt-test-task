{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra where

import           Control.Monad (join)
import           Control.Concurrent (threadDelay)
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Control.Monad.MyExtra
import           Network.Transport.TCP.Address


createEndpointStubbornly :: Address -> IO EndPoint
createEndpointStubbornly expectedAddress@(Address {..}) = untilM $ do
    r <- createTransport addressHost (show addressPort) defaultTCPParameters
    case r of
      Left err | isAlreadyInUseError err -> do
        -- sometimes the OS keeps sockets busy for a minute after a server stops, try again
        printf "local port %d is busy, retrying...\n" addressPort
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left err ->
        fail $ show err
      Right transport -> do
        endpoint <- join $ fromRightM <$> newEndPoint transport
        if address endpoint == endpointAddress expectedAddress
        then
          return $ Just endpoint
        else
          fail $ printf "transport creation succeeded but the resulting address %s isn't the expected %s"
                        (show $ address endpoint)
                        (show $ unparseAddress expectedAddress)

connectStubbornly :: EndPoint -> Address -> IO Connection
connectStubbornly localEndpoint remoteAddress = untilM $ do
    r <- connect localEndpoint (endpointAddress remoteAddress) ReliableOrdered defaultConnectHints
    case r of
      Left (TransportError ConnectNotFound _) -> do
        -- the remote program probably isn't fully-initialized yet, try again.
        printf "remote address %s unreachable, retrying...\n" (unparseAddress remoteAddress)
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left (TransportError _ err) ->
        fail err
      Right connection ->
        return $ Just connection
