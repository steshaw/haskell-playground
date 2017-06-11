#!/usr/bin/env stack
{-
  stack --resolver lts-8.17 script
    --package async
    --package bytestring
    --package text
    --
    -Wall -fwarn-tabs
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent (Chan, newChan, readChan, writeChan)
import Control.Concurrent.Async (mapConcurrently_, concurrently_)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

worker :: Chan (Maybe Int) -> Int -> IO ()
worker chan num = loop
  where
    loop = do
      mi <- readChan chan
      case mi of
        Nothing -> pure ()
        Just i -> do
          say $ pack $ concat ["Worker #", show num, " received value ", show i]
          loop

main :: IO ()
main = do
  chan <- newChan
  let workers = [1 .. 5]
  let nothings = map (const Nothing) workers
  mapConcurrently_ (worker chan) workers `concurrently_`
    mapM_ (writeChan chan) (fmap Just [1 .. 10] ++ nothings)
