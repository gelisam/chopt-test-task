{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Network.Transport.MyExtra
  ( TransportT, runTransportT
  , Event(..)
  , Endpoint, getMyEndpoint, resetMyEndpoint
  , EndpointAddress
  , sendOne, receiveMany
  , createConnectionStubbornly
  
  -- re-export important types from "Network.Transport" so users don't have to also import it
  , Connection, ConnectionId, Transport  -- plus Endpoint and EndpointAddress, exported above
  ) where

import           Control.Lens (makeLenses, use, (.=), (%=))
import           Control.Monad (join)
import           Control.Monad.IO.Class
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
import           Network.Transport.TCP.Address
import           Text.Parsable


data TransportState = TransportState
  { _currentAddress   :: !Address
  , _currentTransport :: !Transport
  , _currentEndpoint  :: !EndPoint
  , _connectionMap    :: !(Map Transport.ConnectionId Address)
  }

makeLenses ''TransportState


type TransportT = StateT TransportState

runTransportT :: Address -> TransportT IO a -> IO a
runTransportT myAddress body = do
    transport0 <- createTransportStubbornly myAddress
    endpoint0 <- createEndpointStubbornly transport0 myAddress
    evalStateT body (TransportState myAddress transport0 endpoint0 mempty)


resetMyEndpoint :: TransportT IO ()
resetMyEndpoint = do
    endpoint <- use currentEndpoint
    liftIO $ Transport.closeEndPoint endpoint
    
    transport <- use currentTransport
    myAddress <- use currentAddress
    endpoint' <- liftIO $ createEndpointStubbornly transport myAddress
    currentEndpoint .= endpoint'



-- Since I'm going to re-export this type anyway, I might as well use my preferred spelling
type Endpoint = EndPoint
type EndpointAddress = EndPointAddress


createTransportStubbornly :: Address -> IO Transport
createTransportStubbornly (Address {..}) = untilJustM $ do
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


createEndpointStubbornly :: Transport -> Address -> IO Endpoint
createEndpointStubbornly transport expectedAddress = do
    endpoint <- join $ fromRightM <$> Transport.newEndPoint transport
    if Transport.address endpoint == unparseEndpointAddress expectedAddress
    then
      return endpoint
    else
      fail $ printf "endpoint creation succeeded but the resulting address %s isn't the expected %s"
                    (show $ Transport.address endpoint)
                    (show $ unparse expectedAddress)


getMyEndpoint :: TransportT IO Endpoint
getMyEndpoint = use currentEndpoint

createConnectionStubbornly :: Endpoint -> Address -> IO (Maybe Connection)
createConnectionStubbornly localEndpoint remoteAddress = untilJustM $ do
    r <- Transport.connect localEndpoint
                           (unparseEndpointAddress remoteAddress)
                           Transport.ReliableOrdered
                           Transport.defaultConnectHints
    case r of
      Left (Transport.TransportError Transport.ConnectNotFound _) -> do
        -- the remote program probably isn't fully-initialized yet, try again.
        printf "remote address %s unreachable, retrying...\n" (unparse remoteAddress)
        threadDelay (1000 * 1000)  -- 1s
        return Nothing
      Left (Transport.TransportError Transport.ConnectFailed "Endpoint closed") ->
        return $ Just Nothing
      Left (Transport.TransportError _ err) ->
        fail err
      Right connection ->
        return $ Just $ Just connection


-- a slightly simpler version of 'Network.Transport.Event'
data Event a
  = Received [a]
  | BrokenConnection Address
  | ClosedConnection Address
  | UnstableEndpoint
  | ClosedEndpoint


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

receiveMany :: Binary a => TransportT IO (Event a)
receiveMany = do
    localEndpoint <- use currentEndpoint
    (liftIO $ Transport.receive localEndpoint) >>= \case
      Transport.Received _ bytestrings -> do
        case traverse Binary.decode bytestrings of
          Nothing -> do
            -- we have received a partial message. this should not happen. but it does.
            -- resetting the endpoint fixes it, tell the caller to do that.
            return UnstableEndpoint
          Just messages ->
            return $ Received messages
      Transport.ConnectionOpened connectionId _ endpointAddress -> do
        -- store the mapping between connectionId and endpointAddress
        address <- parseEndpointAddress endpointAddress
        connectionMap %= Map.insert connectionId address
        
        -- wait for the real messages
        receiveMany
      Transport.ConnectionClosed connectionId -> do
        Just address <- Map.lookup connectionId <$> use connectionMap 
        return $ ClosedConnection address
      Transport.ErrorEvent (Transport.TransportError (Transport.EventConnectionLost lostEndpointAddress) _) ->
        BrokenConnection <$> parseEndpointAddress lostEndpointAddress
      Transport.EndPointClosed -> do
        return ClosedEndpoint
      err -> do
        -- some unexpected event we're not prepared to handle
        fail (show err)
