{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra
  -- export everything from this module
  ( module Network.Transport.MyExtra
  
  -- re-export important types from "Network.Transport" so users don't have to also import it
  , Connection, EndPoint, EndPointAddress, Transport
  ) where

import           Control.Monad (join)
import           Control.Monad.Trans.Resource
import           Control.Concurrent (threadDelay)
import qualified Network.Transport as Transport
import           Network.Transport (Connection, EndPoint, EndPointAddress, Transport)
import qualified Network.Transport.TCP as TCP
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Control.Monad.MyExtra
import qualified Data.Binary.Strict as Binary
import           Data.Binary.Strict (Binary)
import           Network.Transport.TCP.Address
import           Text.Parsable


createTransport :: Address -> ResIO (ReleaseKey, Transport)
createTransport (Address {..}) = allocate go Transport.closeTransport
  where
    go :: IO Transport
    go = untilJustM $ do
        r <- TCP.createTransport addressHost
                                 (show addressPort)
                                 TCP.defaultTCPParameters
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

createEndpoint :: Transport -> Address -> ResIO (ReleaseKey, EndPoint)
createEndpoint transport expectedAddress = allocate go Transport.closeEndPoint
  where
    go :: IO EndPoint
    go = do
        endpoint <- join $ fromRightM <$> Transport.newEndPoint transport
        if Transport.address endpoint == endpointAddress expectedAddress
        then
          return endpoint
        else
          fail $ printf "endpoint creation succeeded but the resulting address %s isn't the expected %s"
                        (show $ Transport.address endpoint)
                        (show $ unparse expectedAddress)

createConnection :: EndPoint -> Address -> ResIO (ReleaseKey, Connection)
createConnection localEndpoint remoteAddress = allocate go Transport.close
  where
    go :: IO Connection
    go = untilJustM $ do
        r <- Transport.connect localEndpoint
                               (endpointAddress remoteAddress)
                               Transport.ReliableOrdered
                               Transport.defaultConnectHints
        case r of
          Left (Transport.TransportError Transport.ConnectNotFound _) -> do
            -- the remote program probably isn't fully-initialized yet, try again.
            printf "remote address %s unreachable, retrying...\n" (unparse remoteAddress)
            threadDelay (1000 * 1000)  -- 1s
            return Nothing
          Left (Transport.TransportError _ err) ->
            fail err
          Right connection ->
            return $ Just connection


sendOne :: Binary a => a -> Connection -> IO ()
sendOne x connection = join $ fromRightM <$> Transport.send connection [Binary.encode x]

receiveMany :: Binary a => EndPoint -> IO (Either Address [a])
receiveMany localEndpoint = Transport.receive localEndpoint >>= \case
    Transport.Received _ messages ->
      return $ Right $ map Binary.decode messages
    Transport.ConnectionOpened {} ->
      -- ignore, wait for the real messages
      receiveMany localEndpoint
    Transport.ErrorEvent (Transport.TransportError (Transport.EventConnectionLost lostEndpointAddress) _) ->
      Left <$> parseEndpointAddress lostEndpointAddress
    err -> do
      -- some unexpected event we're not prepared to handle
      fail (show err)
