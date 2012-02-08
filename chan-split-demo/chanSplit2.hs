module Main where

import Control.Concurrent.Chan.Split
import Control.Concurrent (forkIO)
import Data.Functor.Contravariant
--import Data.CoFunctor

main = do
  (inC, outC) <- newSplitChan
  forkIO $ writeStuffTo inC [1..10]
  getStuffOutOf outC >>=
    mapM_ print . take 10

writeStuffTo :: InChan String -> [Int] -> IO ()
--writeStuffTo = writeList2Chan . cofmap show
writeStuffTo = writeList2Chan . contramap show

getStuffOutOf :: OutChan String -> IO [Int]
getStuffOutOf = getChanContents . fmap read
