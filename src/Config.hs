module Config where

import Control.Distributed.Process.Extras.Time
import Options.Applicative

import Log
import Network.Transport.TCP.Address
import Text.Parsable


data UserProvidedConfig = UserProvidedConfig
  { configMessageSendingDuration :: !TimeInterval
  , configGracePeriodDuration    :: !TimeInterval
  , configRandomSeed             :: !Int
  , configVerbosity              :: !Verbosity
  }
  deriving (Eq, Show)

data FileProvidedConfig = FileProvidedConfig
  { configAddress :: !Address
  }
  deriving (Eq, Show)

data Command
  = CheckArgs !UserProvidedConfig
  | RunNode   !UserProvidedConfig !FileProvidedConfig


parsableOption :: Parsable a => String -> String -> String -> Parser a
parsableOption long_ metavar_ help_ = option (eitherReader parse)
                                    $ long    long_
                                   <> metavar metavar_
                                   <> help    help_

configOption :: Read a => String -> String -> String -> Parser a
configOption long_ metavar_ help_ = option auto
                                  $ long    long_
                                 <> metavar metavar_
                                 <> help    help_


userParser :: Parser UserProvidedConfig
userParser = UserProvidedConfig
         <$> (seconds <$> configOption "send-for"  "SECONDS"      "duration of the message-sending period")
         <*> (seconds <$> configOption "wait-for"  "SECONDS"      "duration of the grace period")
         <*> configOption              "with-seed" "INTEGER"      "fixes all the random decisions"
         <*> configOption              "verbosity" "INTEGER"      "0 for quiet, ..., 4 to trace each communication attempt"

fileParser :: Parser FileProvidedConfig
fileParser = FileProvidedConfig
         <$> parsableOption            "address"   "ADDRESS"      "the node's intended network-transport-tcp address, e.g. \"localhost:8080:0\""

commandParser :: Parser Command
commandParser = subparser
              $ command "check-args" checkArgsInfo
             <> command "run-node"   runNodeInfo


checkArgsInfo :: ParserInfo Command
checkArgsInfo = info (helper <*> (CheckArgs <$> userParser))
              $ fullDesc
             <> progDesc "Verify that the arguments are correct, then exit"

runNodeInfo :: ParserInfo Command
runNodeInfo = info (helper <*> (RunNode <$> userParser <*> fileParser))
            $ fullDesc
           <> progDesc "Exchange messages with other nodes"

commandInfo :: ParserInfo Command
commandInfo = info (helper <*> commandParser)
            $ fullDesc
           <> progDesc "Samuel Gélineau's implementation of the CH/OTP Test Task"
