module Main where

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- least fix point combinator
fix f = f (fix f)

-- Factorial with no recursion.
factorialR f n = if n == 0 then 1 else n * f (n - 1)

factorial' :: Integer -> Integer
factorial' = fix factorialR

main = do
  print (factorial 6, factorial 25)
  print (factorial' 6, factorial' 25)
