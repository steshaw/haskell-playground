module Main where

import Data.Set
import A
import B
import C

test :: Set U
test = ins' X $ ins X $ ins Y $ empty

main :: IO ()
main = print test
