#!/usr/bin/env stack
{-
  stack --resolver lts-8.17 script
    --package async
    --package bytestring
    --package either
    --package mtl
    --package text
    --
    -Wall -fwarn-tabs
-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Control.Concurrent.Async
import Control.Monad (forever)
import Control.Monad.Trans.Either (left, runEitherT)
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)

import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

-- exit = left

worker :: Chan (Maybe Int) -> Int -> IO ()
worker chan num = loop
  where
    loop = do
      mi <- readChan chan
      case mi of
        Nothing -> do
          pure () -- exit!
        Just i -> do
          say $ pack $ concat ["Worker #", show num, " received value ", show i]
          loop -- XXX: What recursion pattern do we have here?

main :: IO ()
main = do
  chan <- newChan
  let workers = [1..5]
  mapConcurrently_ (worker chan) workers `concurrently_`
    writeList2Chan chan (fmap Just [1 .. 10] ++ replicate (length workers) Nothing)
