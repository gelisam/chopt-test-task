module Main where

import           Control.Monad
import           Options.Applicative (execParser)
import           Text.Printf

import           Config (Command(..), commandInfo, FileProvidedConfig(..), UserProvidedConfig(..))
import           Control.Monad.MyExtra
import           Message
import           Network.Transport.MyExtra
import           Network.Transport.TCP.Address
import           Text.Parsable


runNode :: Address -> [Address] -> IO ()
runNode myAddress peerAddresses = do
    endpoint <- createEndpointStubbornly myAddress
    connections <- mapM (connectStubbornly endpoint) peerAddresses
    
    -- send a message to everyone
    myMessage <- randomMessage
    mapM_ (sendOne myMessage) connections
    
    -- receive a message from everyone
    untilTotalM (length connections) $ do
      messages <- receiveMany endpoint :: IO [Message]
      forM_ messages $ \message ->
        printf "%s received %s\n" (unparse myAddress) (show message)
      return (length messages)

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
        let peerAddresses = filter (/= myAddress) allAddresses
        runNode myAddress peerAddresses
