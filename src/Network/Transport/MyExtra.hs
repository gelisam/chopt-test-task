{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra
  -- export everything from this module
  ( module Network.Transport.MyExtra
  
  -- re-export important types from "Network.Transport" so users don't have to also import it
  , Connection, EndPoint, EndPointAddress, Transport
  ) where

import           Control.Monad (join)
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
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


createTransportStubbornly :: Address -> ResIO (ReleaseKey, Transport)
createTransportStubbornly expectedAddress@(Address {..}) = allocate go closeTransport
  where
    go :: IO Transport
    go = untilJustM $ do
        r <- createTransport addressHost (show addressPort) defaultTCPParameters
        case r of
          Left err | isAlreadyInUseError err -> do
            -- sometimes the OS keeps sockets busy for a minute after a server stops, try again
            printf "local port %d is busy, retrying...\n" addressPort
            threadDelay (1000 * 1000)  -- 1s
            return Nothing
          Left err ->
            fail $ show err
          Right transport ->
            return $ Just transport

createEndpointStubbornly :: Transport -> Address -> ResIO (ReleaseKey, EndPoint)
createEndpointStubbornly transport expectedAddress@(Address {..}) = allocate go closeEndPoint
  where
    go :: IO EndPoint
    go = do
        endpoint <- join $ fromRightM <$> newEndPoint transport
        if address endpoint == endpointAddress expectedAddress
        then
          return endpoint
        else
          fail $ printf "endpoint creation succeeded but the resulting address %s isn't the expected %s"
                        (show $ address endpoint)
                        (show $ unparse expectedAddress)

createConnectionStubbornly :: EndPoint -> Address -> ResIO (ReleaseKey, Connection)
createConnectionStubbornly localEndpoint remoteAddress = allocate go close
  where
    go :: IO Connection
    go = untilJustM $ do
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

receiveMany :: Binary a => EndPoint -> IO (Either Address [a])
receiveMany localEndpoint = receive localEndpoint >>= \case
    Received _ messages ->
      return $ Right $ map Binary.decode messages
    ConnectionOpened {} ->
      -- ignore, wait for the real messages
      receiveMany localEndpoint
    ErrorEvent (TransportError (EventConnectionLost lostEndpointAddress) _) ->
      Left <$> parseEndpointAddress lostEndpointAddress
    err -> do
      -- some unexpected event we're not prepared to handle
      fail (show err)
