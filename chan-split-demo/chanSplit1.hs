module Main where

import Control.Concurrent.Chan.Split
import Control.Concurrent (forkIO)

main = do
  (inC, outC) <- newSplitChan
  forkIO $ writeStuffTo inC [1..10]
  getStuffOutOf outC >>=
    mapM_ print . take 10

writeStuffTo :: InChan Int -> [Int] -> IO ()
writeStuffTo = writeList2Chan

getStuffOutOf :: OutChan Int -> IO [Int]
getStuffOutOf = getChanContents
