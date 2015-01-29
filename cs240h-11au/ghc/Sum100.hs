module Sum100 where

sum100n :: Int -> Int
sum100n n = n * foldr (+) 0 [1..100]
