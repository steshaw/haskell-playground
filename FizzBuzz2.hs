{-
 - FizzBuzz solution inspired by http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell
 -}
module Main where

every3 = "" : "" : "Fizz" : every3
every5 = "" : "" : "" : "" : "Buzz" : every5

fizzBuzzes = zipWith (++) every3 every5

numberedFizzBuzzes = zipWith f [1..100] fizzBuzzes
  where
    f n fb = if null fb then show n else fb

main = putStr (unlines numberedFizzBuzzes)
