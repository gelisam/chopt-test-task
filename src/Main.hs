module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Options.Applicative (execParser)

import Config (Config(..), configParserInfo, Role(..))


master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- give the slaves a reasonable amount of time to connect
  liftIO $ threadDelay (2000 * 1000) -- 2s
  
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

main :: IO ()
main = do
    Config messageSendingDuration gracePeriodDuration
           randomSeed
           role
           host port
      <- execParser configParserInfo
    
    case role of
      Master -> do
        backend <- initializeBackend host (show port) initRemoteTable
        startMaster backend (master backend)
      Slave -> do
        backend <- initializeBackend host (show port) initRemoteTable
        startSlave backend
