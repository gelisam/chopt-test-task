module Main where

import Control.Concurrent
import Control.Distributed.Process (NodeId, Process)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Monad
import Control.Monad.IO.Class
import Data.String
import Network.Socket (withSocketsDo)
import Network.Transport
import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Options.Applicative (execParser)

import Config (Command(..), commandInfo, FileProvidedConfig(..), Role(..), UserProvidedConfig(..))


master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
    -- give the slaves a reasonable amount of time to connect
    liftIO $ threadDelay (3000 * 1000) -- 3s
    
    -- Do something interesting with the slaves
    liftIO . putStrLn $ "Slaves: " ++ show slaves
    
    -- Terminate the slaves when the master terminates (this is optional)
    terminateAllSlaves backend
    
    
    -- Network.Transport.TCP's hello world
    liftIO $ do
      serverAddr <- newEmptyMVar
      clientDone <- newEmptyMVar
      
      Right transport <- createTransport "127.0.0.1" "10080" defaultTCPParameters
      
      -- "Server"
      _ <- forkIO $ do
        Right endpoint <- newEndPoint transport
        putMVar serverAddr (address endpoint)
        
        forever $ do
          event <- receive endpoint
          case event of
            Received _ msg -> print msg
            _ -> return () -- ignore
      
      -- "Client"
      _ <- forkIO $ do
        Right endpoint <- newEndPoint transport
        Right conn     <- do addr <- readMVar serverAddr
                             connect endpoint addr ReliableOrdered defaultConnectHints
        _ <- send conn [fromString "Hello world"]
        putMVar clientDone ()
      
      -- Wait for the client to finish
      _ <- takeMVar clientDone
      return ()

main :: IO ()
main = withSocketsDo $ do
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
        backend <- initializeBackend host (show port) initRemoteTable
        case role of
          Master -> startMaster backend (master backend)
          Slave  -> startSlave  backend
