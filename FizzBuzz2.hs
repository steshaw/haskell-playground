{-
 - FizzBuzz solution inspired by http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell
 -}
module Main where

every3 = cycle $ replicate 2 "" ++ ["Fizz"]
every5 = cycle $ replicate 4 "" ++ ["Buzz"]

fizzBuzzes = zipWith (++) every3 every5

numberedFizzBuzzes = zipWith f [1..] fizzBuzzes
  where
    f n fb = if null fb then show n else fb

main = putStr (unlines (take 100 numberedFizzBuzzes))
