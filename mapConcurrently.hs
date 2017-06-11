#!/usr/bin/env stack
{-
  stack --resolver lts-6.23 script
    --package async
    --
    -Wall -fwarn-tabs
-}

-- From https://www.snoyman.com/blog/2016/11/haskells-missing-concurrency-basics

import Control.Concurrent.Async
import Control.Monad (replicateM_, void)

worker :: Int -> IO ()
worker num = replicateM_ 5 $ putStrLn $ "Hi, I'm worker #" ++ show num

main :: IO ()
main = do
    void $ mapConcurrently worker [1..5]
    return ()
