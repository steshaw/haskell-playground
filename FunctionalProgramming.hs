--
-- Inspired by http://vimeo.com/20290504
--

import Data.List (foldl1')
import System.CPUTime (getCPUTime)
import Text.Printf (printf)
import Control.Monad (replicateM)

timeStamp :: IO Integer
timeStamp = getCPUTime

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

computation :: Integer
computation = [1..1000000]
    |> map (* 2)
    |> filter (> 100000)
    |> foldl1' (+) --0

--
-- See http://www.haskell.org/haskellwiki/Timing_computations
--
timeOnce :: IO a -> IO a
timeOnce f = do
  start <- timeStamp
  result <- f
  end <- timeStamp
  let diff = (fromIntegral (end - start)) / (10^9)
  _ <- printf "Computation time: %0.3fms\n" (diff :: Double)
  return result

{-
  Hmmm, in Haskell, I don't think there's a way to repeat this computation many times in order
  to compare the running times… The following attempt doesn't work. The first timing includes
  the time to compute, the rest do not recompute the already computed value…
-}
inIO :: a -> IO a
inIO value = value `seq` return value

timeIt :: IO Integer
timeIt = timeOnce $ inIO computation

timeAndShow :: (Show t) => IO t -> IO ()
timeAndShow a = do
  r <- a
  putStrLn $ "result => " ++ (show r)

main :: IO ()
main = do
  timeAndShow timeIt
  timeAndShow $ replicateM 10 $ timeIt
