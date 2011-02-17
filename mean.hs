module Main where

import Data.List (foldl')

mean :: [Double] -> Double
mean xs = s / fromIntegral l
 where
  (s, l) = foldl' step (0, 0) xs
  step (s, l) a = 
    let s' = s + a
        l' = l + 1
    in seq s' $ seq l' $ (s', l')

main = print $ mean [1..1e8] -- will cause memory alloation failure
