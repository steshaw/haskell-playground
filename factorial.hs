{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Werror #-}

{-# OPTIONS_GHC -Woverflowed-literals #-}

module Main where

import GHC.Natural

factorial :: Natural -> Natural
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- least fix point combinator
fix :: (t -> t) -> t
fix f = f (fix f)

-- Factorial with no recursion.
factorialR :: (Eq p, Num p) => (p -> p) -> p -> p
factorialR f n = if n == 0 then 1 else n * f (n - 1)

factorial' :: Integer -> Integer
factorial' = fix factorialR

main :: IO ()
main = do
  print (factorial 6, factorial 25)
  print (factorial' 6, factorial' 25)
  -- print (factorial (-1)) -- Would be a compile-time error with above GHC
  -- options.
