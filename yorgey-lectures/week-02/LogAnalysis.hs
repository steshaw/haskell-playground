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
parseMessage s = case words s of
  "I":timestamp:ms | goodInt timestamp -> LogMessage Info (toInt timestamp) (unwords ms)
  "W":timestamp:ms | goodInt timestamp -> LogMessage Warning (toInt timestamp) (unwords ms)
  "E":errNum:timestamp:ms | goodInt errNum && goodInt timestamp 
                                       -> LogMessage (Error (toInt errNum)) (toInt timestamp) (unwords ms)
  _                                    -> Unknown s

extractTimestamp :: LogMessage -> TimeStamp
extractTimestamp (LogMessage _ timestamp _) = timestamp
extractTimestamp _ = undefined -- this will never ever happen :)

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) tree           = tree
insert msg Leaf                   = Node Leaf msg Leaf
insert msg (Node left nmsg right) = case compare tmsg tnmsg of
                  								  	LT -> Node (insert msg left) nmsg right
                  								  	_  -> Node left nmsg (insert msg right)
                  								  where
                    									tmsg = extractTimestamp msg
                    									tnmsg = extractTimestamp nmsg

build :: [LogMessage] -> MessageTree
build = foldl (flip insert) Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder t = inOrder0 t []
  where
     inOrder0 Leaf ms = ms
     inOrder0 (Node l m r) ms = inOrder0 l ms ++ [m] ++ inOrder0 r ms

extractString :: LogMessage -> String
extractString (LogMessage _ _ s) = s
extractString _  = undefined

severeError :: LogMessage -> Bool
severeError (LogMessage (Error severity) _ _) | severity >= 50 = True
severeError _                                                  = False

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map extractString . filter severeError . sort
  where sort = inOrder . build

goodInt :: String -> Bool
goodInt s = case (reads s :: [(Int, String)]) of
  [(_, "")] -> True
  _         -> False

toInt :: String -> Int
toInt = read

parse :: String -> [LogMessage]
parse = map parseMessage . lines

type Filename = String

poke :: Filename -> IO [LogMessage]
poke = testParse parse 100

pokeIt :: Filename -> IO ()
pokeIt filename = do
  msgs <- poke filename
  print $ whatWentWrong msgs

pokeError :: IO ()
pokeError = pokeIt "error.log"

pokeSample :: IO ()
pokeSample = pokeIt "sample.log"

test :: Filename -> IO [String]
test = testWhatWentWrong parse whatWentWrong

testError :: IO [String]
testError = test "error.log"

testSample :: IO [String]
testSample = test "sample.log"

test' :: Show a => IO [a] -> IO ()
test' x = x >>= mapM_ print

testError' :: IO ()
testError' = test' testError

testSample' :: IO ()
testSample' = test' testSample
