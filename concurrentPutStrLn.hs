#!/usr/bin/env stack
{-
  stack --resolver lts-8.17 script
    --package async
    --package text
    --
    -Wall -fwarn-tabs
-}

-- From https://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics

import Control.Concurrent.Async
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.IO

worker1 :: Int -> IO ()
worker1 num =
  replicateM_ 5 $ putStrLn $ "Hi, I'm worker #" ++ show num

worker2 :: Int -> IO ()
worker2 num =
  replicateM_ 5
    $ T.putStrLn
    $ T.pack
    $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
  when True $ hSetBuffering stdout LineBuffering
  when True $ do
    mapConcurrently_ worker1 [1..5]
    putStrLn "--"
  when True $ mapConcurrently_ worker2 [1..5]
  return ()
