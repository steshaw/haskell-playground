{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log

--
-- lines, words, unwords, take, drop, and (.).
--

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
parseMessage s = case (words s) of
  "I":timestamp:ms -> LogMessage Info (toInt timestamp) (unwords ms)
  "W":timestamp:ms -> LogMessage Warning (toInt timestamp) (unwords ms)
  "E":errNum:timestamp:ms -> LogMessage (Error (toInt errNum)) (toInt timestamp) (unwords ms)
  _ -> Unknown s

toInt :: String -> Int
toInt s = read s -- FIX handle error


parse :: String -> [LogMessage]
parse = map parseMessage . lines

poke :: IO [LogMessage]
poke = testParse parse 10 "error.log"

poke' :: IO ()
poke' = poke >>= mapM_ (putStrLn . show)
