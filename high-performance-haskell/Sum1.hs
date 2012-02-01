module Sum1 where

import Prelude hiding (sum)

sum :: [Int] -> Int
sum []   = 0
sum (x:xs) = x + sum xs
