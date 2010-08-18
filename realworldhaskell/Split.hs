module Split where

import Data.Char
import Control.Applicative

splitWith :: (a -> Bool) -> [a] -> [[a]]
splitWith p [] = []
splitWith p xs =
  let (pre, suf) = break p xs
  in pre:(splitWith p (dropWhile p suf))

split = splitWith isSpace

split' = splitWith ((||) <$> isSpace <*> isPunctuation)
