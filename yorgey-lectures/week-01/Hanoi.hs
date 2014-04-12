module Hanoi where

type Peg = String
type Move = (Peg, Peg)

-- |
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"), ("a","b"), ("c","b")] 1234
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi = undefined
