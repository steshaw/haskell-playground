#!/usr/bin/env stack
{-
  stack --resolver lts-8.17 script
    --package async
    --package bytestring
    --package text
    --
    -Wall -fwarn-tabs
-}
import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

worker :: Chan Int -> Int -> IO ()
worker chan num =
  forever $ do
    i <- readChan chan
    say $ pack $ concat ["Worker #", show num, " received value ", show i]

main :: IO ()
main = do
  chan <- newChan
  mapConcurrently_ (worker chan) [1 .. 5] `concurrently_`
    writeList2Chan chan [1 .. 10]
