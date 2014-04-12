{-# OPTIONS_GHC -Wall #-}
module CreditCard where

import Utils ((|>))

-- |
-- >>> toDigitsRev 1234
-- [4,3,2,1]
-- >>> toDigits 1234
-- [1,2,3,4]
-- >>> toDigits 0
-- []
-- >>> toDigits (-17)
-- []
--
toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = if d == 0 then [m] else m : toDigitsRev d
                  where
                    (d, m) = n `divMod` 10

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev

-- |
-- >>> doubleEveryOther [8, 7, 6, 5]
-- [16,7,12,5]
-- >>> doubleEveryOther [1, 2, 3]
-- [1,4,3]
--
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEm . reverse
  where
    doubleEm :: [Integer] -> [Integer]
    doubleEm (a:b:xs) = a : (b * 2) : doubleEm xs
    doubleEm xs = xs

-- |
-- >>> sumDigits [] 
-- 0
-- >>> sumDigits [16] 
-- 7
-- >>> sumDigits [16,7,12,5] 
-- 22
--
sumDigits :: [Integer] -> Integer
sumDigits xs = sum $ xs >>= toDigits

-- |
-- >>> validate 4012888888881881
-- True
-- >>> validate 4012888888881882
-- False
--
validate :: Integer -> Bool
validate n = 
       toDigits n 
    |> doubleEveryOther
    |> sumDigits |> validSum
  where
    validSum m = m `mod` 10 == 0
