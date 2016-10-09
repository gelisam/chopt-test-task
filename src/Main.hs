module Main where

import           Control.Monad
import           Data.List
import           Options.Applicative (execParser)
import           Text.Printf

import           Config (Command(..), commandInfo, FileProvidedConfig(..), UserProvidedConfig(..))
import           Message
import           Network.Transport.MyExtra
import           Network.Transport.TCP.Address
import           Text.Parsable


runNode :: Int -> Int -> Address -> [Address] -> IO ()
runNode nbNodes myIndex myAddress peerAddresses = do
    endpoint <- createEndpointStubbornly myAddress
    connections <- mapM (connectStubbornly endpoint) peerAddresses
    
    forM_ [0..] $ \roundNumber -> do
      message <- runRound nbNodes myIndex endpoint connections roundNumber
      printf "node #%d agrees: round %d's message is %s\n" myIndex roundNumber (show message)

main :: IO ()
main = do
    command <- execParser commandInfo
    case command of
      CheckArgs _ ->
        -- if the arguments were incorrect, 'execParser' would have complained.
        return ()
      RunNode (UserProvidedConfig messageSendingDuration gracePeriodDuration
                                  randomSeed)
              (FileProvidedConfig myAddress)
              -> do
        allAddresses <- join $ mapM parse <$> lines <$> readFile "nodelist.txt"
        let nbNodes = length allAddresses
        myIndex <- case elemIndex myAddress allAddresses of
          Just x -> return x
          Nothing -> fail $ printf "%s is a valid address but it is not listed in nodelist.txt" (show myAddress)
        let peerAddresses = filter (/= myAddress) allAddresses
        runNode nbNodes myIndex myAddress peerAddresses
