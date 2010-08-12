module Main where

import Data.Char(toUpper)

main :: IO ()
main = getContents >>= (\ contents -> putStr $ map toUpper contents)
