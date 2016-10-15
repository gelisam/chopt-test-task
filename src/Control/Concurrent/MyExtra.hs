module Control.Concurrent.MyExtra where

import Control.Concurrent
import Control.Distributed.Process.Extras.Time
import Data.Time


sleepUntil :: UTCTime -> IO ()
sleepUntil stopTime = do
    currentTime <- getCurrentTime
    
    let timeoutDelay = asTimeout
                     $ diffTimeToTimeInterval
                     $ diffUTCTime stopTime currentTime
    threadDelay timeoutDelay  -- returns immediately if 'timeoutDelay' is negative
