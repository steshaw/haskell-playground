module Factorial where

factorial 0 = 1
factorial n = n * factorial (n-1)

main = print (factorial 6, factorial 25)
