{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.Transport.MyExtra
  -- export everything from this module
  ( module Network.Transport.MyExtra
  
  -- re-export important types from "Network.Transport" so users don't have to also import it
  , Connection, ConnectionId, Transport  -- plus Endpoint and EndpointAddress, exported above
  ) where

import           Control.Monad (join)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Monad.Trans.State.Strict
import           Control.Concurrent (threadDelay)
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import qualified Network.Transport as Transport
import           Network.Transport (Connection, ConnectionId, EndPoint, EndPointAddress, Transport)
import qualified Network.Transport.TCP as TCP
import           System.IO.Error (isAlreadyInUseError)
import           Text.Printf

import           Control.Monad.MyExtra
import qualified Data.Binary.Strict as Binary
import           Data.Binary.Strict (Binary)
import           Log
import           Network.Transport.TCP.Address
import           Text.Parsable


-- Since I'm going to re-export this type anyway, I might as well use my preferred spelling
type Endpoint = EndPoint
type EndpointAddress = EndPointAddress


createUnprotectedTransport :: Verbosity -> Address -> IO Transport
createUnprotectedTransport verbosity (Address {..}) = untilJustM $ do
    r <- TCP.createTransport addressHost
                             (show addressPort)
                             TCP.defaultTCPParameters
    case r of
      Left err | isAlreadyInUseError err -> do
        -- sometimes the OS keeps sockets busy for a minute after a server stops, try again
        putLogLn verbosity 1
               $ printf "local port %d is busy, retrying...\n" addressPort
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left err ->
        fail $ show err
      Right transport ->
        return $ Just transport

createTransport :: Verbosity -> Address -> ResIO (ReleaseKey, Transport)
createTransport verbosity myAddress = flip allocate Transport.closeTransport
                                    $ createUnprotectedTransport verbosity myAddress


createUnprotectedEndpoint :: Transport -> Address -> IO Endpoint
createUnprotectedEndpoint transport expectedAddress = do
    endpoint <- join $ fromRightM <$> Transport.newEndPoint transport
    if Transport.address endpoint == unparseEndpointAddress expectedAddress
    then
      return endpoint
    else
      fail $ printf "endpoint creation succeeded but the resulting address %s isn't the expected %s"
                    (show $ Transport.address endpoint)
                    (show $ unparse expectedAddress)

createEndpoint :: Transport -> Address -> ResIO (ReleaseKey, Endpoint)
createEndpoint transport expectedAddress = flip allocate Transport.closeEndPoint
                                         $ createUnprotectedEndpoint transport expectedAddress


createUnprotectedConnection :: Verbosity -> Endpoint -> Address -> IO Connection
createUnprotectedConnection verbosity localEndpoint remoteAddress = untilJustM $ do
    r <- Transport.connect localEndpoint
                           (unparseEndpointAddress remoteAddress)
                           Transport.ReliableUnordered
                           Transport.defaultConnectHints
    case r of
      Left (Transport.TransportError Transport.ConnectNotFound _) -> do
        -- the remote program probably isn't fully-initialized yet, try again.
        putLogLn verbosity 1
               $ printf "remote address %s unreachable, retrying...\n" (unparse remoteAddress)
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left (Transport.TransportError _ err) ->
        fail err
      Right connection ->
        return $ Just connection

createConnection :: Verbosity -> Endpoint -> Address -> ResIO (ReleaseKey, Connection)
createConnection verbosity localEndpoint remoteAddress = flip allocate Transport.close
                                                       $ createUnprotectedConnection verbosity localEndpoint remoteAddress


-- a slightly simpler version of 'Network.Transport.Event'
data Event a
  = Received [a]
  | BrokenConnection Address
  | ClosedConnection Address
  | ClosedEndpoint


-- 'ConnectionOpened' and 'ConnectionClosed' require us to keep track of ConnectionIds
type TransportT = StateT (Map Transport.ConnectionId Address)

runTransportT :: Monad m => TransportT m a -> m a
runTransportT = flip evalStateT mempty


sendOne :: Binary a => a -> Connection -> IO ()
sendOne x connection = do
    r <- Transport.send connection [Binary.encode x]
    case r of
      Right () ->
        return ()
      Left (Transport.TransportError Transport.SendClosed _) ->
        -- ignore, 'receiveMany' will get also get a 'ConnectionClosed' and will deal with it
        return ()
      Left (Transport.TransportError Transport.SendFailed _) ->
        -- ignore, 'receiveMany' will get also get a 'EventConnectionLost' and will deal with it
        return ()

receiveMany :: Binary a => Endpoint -> TransportT IO (Event a)
receiveMany localEndpoint = (liftIO $ Transport.receive localEndpoint) >>= \case
    Transport.Received _ messages ->
      return $ Received $ map Binary.decode messages
    Transport.ConnectionOpened connectionId _ endpointAddress -> do
      -- store the mapping between connectionId and endpointAddress
      address <- parseEndpointAddress endpointAddress
      modify $ Map.insert connectionId address
      
      -- wait for the real messages
      receiveMany localEndpoint
    Transport.ConnectionClosed connectionId -> do
      Just address <- Map.lookup connectionId <$> get
      return $ ClosedConnection address
    Transport.ErrorEvent (Transport.TransportError (Transport.EventConnectionLost lostEndpointAddress) _) ->
      BrokenConnection <$> parseEndpointAddress lostEndpointAddress
    Transport.EndPointClosed -> do
      return ClosedEndpoint
    err -> do
      -- some unexpected event we're not prepared to handle
      fail (show err)
