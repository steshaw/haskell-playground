#!/usr/bin/env stack
{-
  stack --resolver lts-8.17 script
    --package async
    --package bytestring
    --package stm
    --package stm-chans
    --package text
    --
    -Wall -fwarn-tabs
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TMQueue
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

worker :: TMQueue Int -> Int -> IO ()
worker q num = loop
  where
    loop = do
      mi <- atomically $ readTMQueue q
      case mi of
        Nothing -> pure ()
        Just i -> do
          say $ pack $ concat ["Worker #", show num, " received value ", show i]
          loop

main :: IO ()
main = do
  q <- newTMQueueIO
  mapConcurrently (worker q) [1 .. 5] `concurrently_` do
    mapM_ (atomically . writeTMQueue q) [1 .. 10]
    atomically $ closeTMQueue q
