module Fibs where

-- Seems much faster (in ghci anyhow) than fibs1
fibs :: [Integer]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibs1 :: Num a => [a]
fibs1 = 0 : 1 : zipWith (+) fibs1 (tail fibs1)
