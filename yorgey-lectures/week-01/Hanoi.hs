{-# OPTIONS_GHC -Wall #-}
module Hanoi where

type Peg = String
type Move = (Peg, Peg)

{-

  Hanoi:

- general:

          move n     a b using c
    => 1. move (n-1) a c using b
       2. move top disc from a to b
       3. move (n-1) from c to b (using a)

- When there is 1:

          move 1 a->b (using c)
    => 1. move 0 a->c (using b)
       2. move top disc from a->b
       3. move 0 c->b (using a)

  - When there is 2:
          move 2 a->b (using c)
    => 1. move 1 a->c (using b)
       2. move top disc from a->b
       3. move 1 c->b (using a)
-}
-- |
-- >>> hanoi 2 "a" "b" "c"
-- [("a","c"),("a","b"),("c","b")]
-- >>> hanoi 1 "a" "b" "c"
-- [("a","b")]
-- >>> hanoi 0 "a" "b" "c"
-- []
-- >>> (length $ hanoi 15 "a" "b" "c") == 2^15 - 1
-- True
--
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi 1 a b _ = [(a, b)]
hanoi n a b c =
  hanoi (n-1) a c b ++
  hanoi 1 a b undefined ++
  hanoi (n-1) c b a

