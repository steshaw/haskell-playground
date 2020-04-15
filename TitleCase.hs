module TitleCase where

import Data.Char

titleCase :: String -> String
titleCase s = unwords (map initCase (words s))
  where
    initCase :: String -> String
    initCase (x : xs) = x : fmap toLower xs
    initCase [] = []
