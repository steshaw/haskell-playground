module Main where

primes = sieve [2..]
  where
    sieve (p:xs) = p:sieve [x|x <- xs, x `mod` p > 0]

main :: IO ()
main = do
  print (take 10 primes)
