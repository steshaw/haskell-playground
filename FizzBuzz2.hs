{-
 - FizzBuzz solution inspired by http://pragprog.com/magazines/2012-08/thinking-functionally-with-haskell
 -}
module Main where

import Data.Monoid (mappend)

every3 = cycle $ replicate 2 Nothing ++ [Just "Fizz"]
every5 = cycle $ replicate 4 Nothing ++ [Just "Buzz"]

fizzBuzzes = zipWith mappend every3 every5

numberedFizzBuzzes = zipWith f [1..] fizzBuzzes
  where
    f n fb = maybe (show n) id fb

main = putStr (unlines (take 100 numberedFizzBuzzes))
