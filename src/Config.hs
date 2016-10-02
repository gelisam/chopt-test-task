module Config where

import Options.Applicative


data Role
  = Master
  | Slave
  deriving (Eq, Read, Show)

data Config = Config
  { configMessageSendingDuration :: Int
  , configGracePeriodDuration :: Int
  , configRandomSeed :: Int
  , configRole :: Role
  , configHost :: String
  , configPort :: Int
  }
  deriving (Eq, Show)


stringOption :: String -> String -> String -> Parser String
stringOption long_ metavar_ help_ = strOption
                                  $ long    long_
                                 <> metavar metavar_
                                 <> help    help_

configOption :: Read a => String -> String -> String -> Parser a
configOption long_ metavar_ help_ = option auto
                                  $ long    long_
                                 <> metavar metavar_
                                 <> help    help_

configParser :: Parser Config
configParser = Config
           <$> configOption "send-for"  "SECONDS"               "duration of the message-sending period"
           <*> configOption "wait-for"  "SECONDS"               "duration of the grace period"
           <*> configOption "with-seed" "INTEGER"               "fixes all the random decisions"
           <*> configOption "role"      "Master|Slave"          "exactly one node should be the master"
           <*> stringOption "host"      "localhost|IP|HOSTNAME" "hostname and port via which other nodes"
           <*> configOption "port"      "INTEGER"               "will contact this node"

configParserInfo :: ParserInfo Config
configParserInfo = info (helper <*> configParser)
                 $ fullDesc
                <> progDesc "Exchanges messages with other nodes as per the CH/OTP Test Task"
