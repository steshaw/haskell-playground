module Sum100StrictFoldLeft where

import Data.List (foldl')

sum100n :: Int -> Int
sum100n n = n * foldl' (+) 0 [1..100]
