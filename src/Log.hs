module Log where


type Verbosity = Int

putLogLn :: Verbosity -> Verbosity -> String -> IO ()
putLogLn maximumLevel messageLevel message | messageLevel <= maximumLevel = putStrLn message
                                           | otherwise                    = return ()
