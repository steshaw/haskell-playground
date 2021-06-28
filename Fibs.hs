{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Werror #-}
{-# OPTIONS_GHC -Widentities #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
{-# OPTIONS_GHC -Wpartial-fields #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Fibs (main) where

import Prelude hiding (putStrLn, fromIntegral)

import Data.Text (Text)
import Data.Text.IO (putStrLn)
import qualified Data.List as L
import qualified Data.Text as T
import qualified Prelude as P

-- Seems much faster (in ghci anyhow) than _fibsIntegral and _fibsNum.
_fibsInteger :: [Integer]
_fibsInteger = 0 : 1 : zipWith (+) _fibsInteger (tail _fibsInteger)

_fibsIntegral :: Integral a => [a]
_fibsIntegral = 0 : 1 : zipWith (+) _fibsIntegral (tail _fibsIntegral)

_fibsNum :: Num a => [a]
_fibsNum = 0 : 1 : zipWith (+) _fibsNum (tail _fibsNum)

unsafeFromIntegralXXX :: (Integral a, Num b) => a -> b
unsafeFromIntegralXXX = P.fromIntegral

tshow :: Show a => a -> Text
tshow = T.pack . show

fibA :: Integer -> Integer
fibA 0 = 0
fibA 1 = 1
fibA n = fibA (n - 1) + fibA (n - 2)

fibB :: Integer -> Integer
fibB index = foobs !! unsafeFromIntegralXXX index
  where
    foobs = 0 : 1 : zipWith (+) foobs (tail foobs)

someFibs :: [Integer]
someFibs = [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144]

check :: Text -> (Integer -> Integer) -> IO ()
check name fib = do
  let n :: Integer = L.genericLength someFibs
  let actual = take (unsafeFromIntegralXXX n) (map fib [0 .. n])
  putStrLn $ "Testing " <> name
  if actual /= someFibs then do
    putStrLn $ "Expected: " <> tshow someFibs
    putStrLn $ "Actual  : " <> tshow actual
  else
    putStrLn "ok"

main :: IO ()
main = do
  check "fibA" fibA
  check "fibB" fibB
