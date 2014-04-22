{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

-- |
-- >>> parseMessage "I 29 la la la"
-- LogMessage Info 29 "la la la"
-- >>> parseMessage "W 99 Warning Will Robinson. Warning!"
-- LogMessage Warning 99 "Warning Will Robinson. Warning!"
-- >>> parseMessage "E 2 562 help help"
-- LogMessage (Error 2) 562 "help help"
-- >>> parseMessage "This is not in the right format"
-- Unknown "This is not in the right format"
--
parseMessage :: String -> LogMessage
parseMessage _ = Unknown "muhahaha"

parse :: String -> [LogMessage]
parse = map parseMessage . lines

poke :: IO [LogMessage]
poke = testParse parse 10 "error.log"

printPoke :: IO ()
printPoke = poke >>= mapM_ (putStrLn . show)
