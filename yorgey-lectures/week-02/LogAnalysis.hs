{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where

import Log
import Control.Applicative
import Data.Maybe (fromMaybe)

parseInt :: String -> Maybe Int
parseInt s = case (reads s :: [(Int, String)]) of
  [(n, "")] -> Just n
  _         -> Nothing

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
parseMessage s = fromMaybe (Unknown s) $ case words s of
  "I":timestamp:ms        -> mkMessage Info <$> parseInt timestamp <*> pure ms
  "W":timestamp:ms        -> mkMessage Warning <$> parseInt timestamp <*> pure ms
  "E":errNum:timestamp:ms -> mkMessage <$> mkError errNum <*> parseInt timestamp <*> pure ms
  _                       -> Nothing
  where
    mkMessage msgType timestamp ms = LogMessage msgType timestamp (unwords ms)
    mkError errNum = Error <$> parseInt errNum

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
build = foldr insert Leaf

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

-- |
-- >>> testSample
-- ["Way too many pickles","Bad pickle-flange interaction detected","Flange failed!"]
--
testSample :: IO [String]
testSample = test "sample.log"

testError :: IO [String]
testError = test "error.log"

test' :: Show a => IO [a] -> IO ()
test' x = x >>= mapM_ print

testSample' :: IO ()
testSample' = test' testSample

testError' :: IO ()
testError' = test' testError
