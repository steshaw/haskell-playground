module Golf where

import Data.List
import qualified Data.Map as M
import Control.Arrow

--
-- Exercise 1 Hopscotch
--

skips :: [a] -> [[a]]
skips xs = map (map snd . f . fst) ps
  where
    f i = filter (m i) ps
    m i (n, _) = mod n i == 0
    ps = zip [1 :: Integer ..] xs

-- original skips
skips0 :: [a] -> [[a]]
skips0 xs = map (everyNth xs) [1 .. length xs]
  where
    everyNth :: [a] -> Int -> [a]
    everyNth as n
      | n <= length as = last (take n as) : everyNth (drop n as) n
      | otherwise      = []

-- tests
skipsT1 :: Bool
skipsT1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
skipsT2 :: Bool
skipsT2 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skipsT3 :: Bool
skipsT3 = skips [1 :: Int] == [[1]]
skipsT4 :: Bool
skipsT4 = skips [True, False] == [[True, False], [False]]
skipsT5 :: Bool
skipsT5 = null $ skips ([] :: [Int])
skipsTests :: [Bool]
skipsTests = [skipsT1, skipsT2, skipsT3, skipsT4, skipsT5]
skipsAll :: Bool
skipsAll = all (== True) skipsTests


--
-- Exercise 2 Local maxima
--

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : ns)
  | a < b && b > c = b : localMaxima (b:c:ns)
  | otherwise      = localMaxima (b:c:ns)
localMaxima _ = []

-- tests

localMaxima1 :: Bool
localMaxima1 = localMaxima [2, 9, 5, 6, 1] == [9, 6]
localMaxima2 :: Bool
localMaxima2 = localMaxima [2, 3, 4, 1, 5] == [4]
localMaxima3 :: Bool
localMaxima3 = null $ localMaxima [1,2,3,4,5]
localMaximaTests :: [Bool]
localMaximaTests = [localMaxima1, localMaxima2, localMaxima3]
localMaximaAll :: Bool
localMaximaAll = all (== True) localMaximaTests


--
-- Exercise 3 Histogram
--

histogram :: [Integer] -> String
histogram ns = unlines $ grid ++ key
  where
    grid = [[ c occ $ M.lookup i m | i <- [0 .. 9]] | occ <- reverse [1 .. maxOcc]]
      where
        ps = map ((!! 0) &&& length) $ group $ sort ns
        m = M.fromList ps
        maxOcc = maximum $ map snd ps
        c count (Just occurrence) | count <= occurrence = '*'
        c _ _                                          = ' '
    key = [ "=========="
          , "0123456789"]

-- tests

r1 :: [String]
r1 =
  [" *        "
  ," *        "
  ," *   *    "
  ,"=========="
  ,"0123456789"
  ]
histogram1 :: Bool
histogram1 = histogram [1,1,1,5] == unlines r1
r2 :: [String]
r2 =
  ["    *     "
  ,"    *     "
  ,"    * *   "
  ," ******  *"
  ,"=========="
  ,"0123456789"
  ]
histogram2 :: Bool
histogram2 = histogram [1,4,5,4,6,6,3,4,2,4,9] == unlines r2
histogram3 :: Bool
histogram3 = histogram [3,5] == "   * *    \n==========\n0123456789\n"
histogramTests :: [Bool]
histogramTests = [histogram1, histogram2, histogram3]
histogramAll :: Bool
histogramAll = all (== True) histogramTests
