module Main where

import Data.Char (toUpper)

main = interact (unlines . fmap someLineWiseFunction . lines)

someLineWiseFunction = map toUpper
