{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra where

import           Control.Monad (join)
import           Control.Concurrent (threadDelay)
import           Network.Transport
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Control.Monad.MyExtra
import qualified Data.Binary.Strict as Binary
import           Data.Binary.Strict (Binary)
import           Network.Transport.TCP.Address
import           Text.Parsable


createEndpointStubbornly :: Address -> IO EndPoint
createEndpointStubbornly expectedAddress@(Address {..}) = untilJustM $ do
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
                        (show $ unparse expectedAddress)

connectStubbornly :: EndPoint -> Address -> IO Connection
connectStubbornly localEndpoint remoteAddress = untilJustM $ do
    r <- connect localEndpoint (endpointAddress remoteAddress) ReliableOrdered defaultConnectHints
    case r of
      Left (TransportError ConnectNotFound _) -> do
        -- the remote program probably isn't fully-initialized yet, try again.
        printf "remote address %s unreachable, retrying...\n" (unparse remoteAddress)
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left (TransportError _ err) ->
        fail err
      Right connection ->
        return $ Just connection


sendOne :: Binary a => a -> Connection -> IO ()
sendOne x connection = join $ fromRightM <$> send connection [Binary.encode x]

receiveMany :: Binary a => EndPoint -> IO [a]
receiveMany localEndpoint = receive localEndpoint >>= \case
    Received _ messages ->
      return $ map Binary.decode messages
    ConnectionOpened {} ->
      -- ignore, wait for the real messages
      receiveMany localEndpoint
    err -> do
      fail (show err)
