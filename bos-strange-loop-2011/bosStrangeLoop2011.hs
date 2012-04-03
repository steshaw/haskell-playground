--
-- See http://bos.github.com/strange-loop-2011/
--

import Data.Char (isUpper, isLower, toLower)

import Prelude hiding (filter)
import Control.Arrow ((>>>))

data Foo = One | Two | Three
  deriving (Show, Enum)

myLength :: Num n => [a] -> n
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

countCaps :: [Char] -> Integer
countCaps [] = 0
countCaps (c:cs) = (if isUpper c then 1 else 0) + countCaps cs

bosCountCaps :: [Char] -> Integer
bosCountCaps []    = 0
bosCountCaps (x:xs)
   | isUpper x      = 1 + bosCountCaps xs
   | otherwise      = bosCountCaps xs

caps :: [Char] -> [Char]
caps []     = []
caps (c:cs) = if isUpper c
              then c : caps cs
              else caps cs

countCaps2 :: [Char] -> Integer
countCaps2 xs = myLength $ caps xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []    = []
filter p (x:xs)
  | p x        = x : filter p xs
  | otherwise  =     filter p xs

countLowerCase :: (Num n) => [Char] -> n
countLowerCase = filter isLower >>> myLength

foo :: Integer
foo = let x = 2
          y = 4
      in x + y

-- Remove words from a sentence that do not start with a vowel.
disemvowel :: String -> String
disemvowel = unwords . (filter startsWithVowel) . words
  where
    startsWithVowel [] = False
    startsWithVowel (c:_) = (toLower c) `elem` vowels
    vowels = "aeiuo"

bosDisEmvowel =
  let isVowel c = toLower c `elem` "aeiou"
  in  unwords . filter (isVowel . head) . words
