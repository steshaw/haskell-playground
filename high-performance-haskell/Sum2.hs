module Sum2 where

import Prelude hiding (sum)

sum :: [Int] -> Int
sum = sum' 0
  where
    sum' acc []     = 0
    sum' acc (x:xs) = sum' (acc + x) xs
