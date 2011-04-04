module Main where

import Data.Char (toUpper)

main :: IO ()
main = interact (unlines . fmap someLineWiseFunction . lines)

someLineWiseFunction :: String -> String
someLineWiseFunction = map toUpper
