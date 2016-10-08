module Message where

import System.Random


type Message = Double

randomMessage :: IO Message
randomMessage = randomRIO (0, 1)
