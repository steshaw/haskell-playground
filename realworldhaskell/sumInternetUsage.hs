--
-- Extracts internet usage from file of columns. Peak is in column 7 and off-peak in column 8.
--
-- To run just feed it the internet usage file:
--
--   $ sumInternetUsage <internet-usage.txt
--
module Main where

import Steshaw
import Split
import Control.Applicative

col :: Int -> [[String]] -> [Double]
col n = liftA $ flip (!!) (n-1) >.> read

columns :: String -> [[String]]
columns = lines >.> liftA split

data Usage = Usage {peak :: Double, offPeak :: Double} deriving Show

sumPeakAndOffPeak :: String -> Usage
sumPeakAndOffPeak s = Usage (sum (col 7 cols)) (sum (col 8 cols))
  where cols = columns s

main = do
  interact $ sumPeakAndOffPeak >.> show
  putStrLn ""
