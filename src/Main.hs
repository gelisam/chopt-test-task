module Main where

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Distributed.Process.Node (initRemoteTable)
import Options.Applicative (execParser)

import Config (Command(..), commandInfo, FileProvidedConfig(..), Role(..), UserProvidedConfig(..))


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
