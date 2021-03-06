module Main where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Data.List
import           Data.Time
import           Options.Applicative (execParser)
import           Text.Printf

import           Algorithm
import           Config (Command(..), commandInfo, FileProvidedConfig(..), UserProvidedConfig(..))
import           Interpreter
import           Network.Transport.MyExtra
import           Text.Parsable


main :: IO ()
main = do
    startTime <- getCurrentTime
    command <- execParser commandInfo
    case command of
      CheckArgs _ ->
        -- if the arguments were incorrect, 'execParser' would have complained.
        return ()
      RunNode userConfig (FileProvidedConfig myAddress) -> do
        allAddresses <- join $ mapM parse <$> lines <$> readFile "node_list.txt"
        myIndex <- case elemIndex myAddress allAddresses of
          Just x -> return x
          Nothing -> fail $ printf "%s is a valid address but it is not listed in node_list.txt" (show myAddress)
        
        runResourceT $ do
          transport   <- snd <$> createTransport (configVerbosity userConfig) myAddress
          endpoint    <- snd <$> createEndpoint transport myAddress
          
          lift $ interpret userConfig startTime myIndex myAddress allAddresses endpoint algorithm
