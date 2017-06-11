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
import Control.Monad.Trans
import Control.Monad.Trans.Either
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Void

import qualified Data.ByteString.Char8 as S8

say :: Text -> IO ()
say = S8.putStrLn . encodeUtf8

exit :: e -> EitherT e IO a
exit = left

worker :: Chan (Maybe Int) -> Int -> IO (Either () Void)
worker chan num =
  runEitherT $
  forever $ do
    mi <- lift $ readChan chan
    case mi of
      Nothing -> do
        exit ()
      Just i -> do
        lift $
          say $ pack $ concat ["Worker #", show num, " received value ", show i]

main :: IO ()
main = do
  chan <- newChan
  let workers = [1 .. 5]
  let nothings = replicate (length workers) Nothing
  mapConcurrently_ (worker chan) workers `concurrently_`
    writeList2Chan chan (fmap Just [1 .. 10] ++ nothings)
