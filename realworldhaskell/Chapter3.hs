{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Chapter3 where

import Steshaw ((>>>), (>.>))
import Tree
import Data.Monoid
import Data.List(sortBy)
import qualified Data.Foldable as F

len1 :: [a] -> Integer
len1 [] = 0
len1 (x:xs) = 1 + (len1 xs)

len2 :: [a] -> Integer
len2 xs = foldl (+) 0 $ map (\n -> 1) xs

len3 :: [a] -> Integer
len3 xs = foldl (\ acc n -> acc + 1) 0 xs

len4 :: (F.Foldable foldable, Num n) => foldable a -> n
len4 xs = F.foldMap (\n -> Sum 1) xs >>> getSum

meanOverFractionals :: (Fractional a) => [a] -> a
meanOverFractionals xs = sum xs / len4 xs

meanOverIntegrals :: (Integral a, Fractional b) => [a] -> b
meanOverIntegrals xs = (fromIntegral $ sum xs) / len4 xs

class (Num a) => ToFractional a where
  toFractional :: (Fractional b) => a -> b

{-
--
-- Needs extensions FlexibleInstances + UndecidableInstances.
--
instance (Integral a) => ToFractional a where
  toFractional = fromIntegral
-}

{-
instance ToFractional Integer where
  toFractional = fromIntegral

instance ToFractional Int where
  toFractional = fromIntegral
-}

{-
--
-- FIXME: Causes duplicate with instance (Integral a) => ToFractional a
--
instance (Fractional a) => ToFractional a where
  toFractional = id
-}

toFrac :: (Real a, Fractional b) => a -> b
toFrac = toRational >.> fromRational

instance (Real n) => ToFractional n where
  toFractional = realToFrac
--  toFractional = toRational >.> fromRational
{-
instance ToFractional Double where
  toFractional = id
instance ToFractional Float where
  toFractional = id
-}

mean3 :: (ToFractional a, Fractional b) => [a] -> b
mean3 xs = (toFractional $ sum xs) / (len4 xs)

mean4 :: (Real a, Fractional b) => [a] -> b
mean4 xs = (realToFrac $ sum xs) / (len4 xs)

mkPalindrome xs = xs ++ (reverse xs)

isPalindrome xs = xs == (reverse xs)

sortBySubLength xs = sortBy (\a b -> length a `compare` length b) xs

intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse a xs = foldl1 (\acc b -> acc ++ (a:b)) xs

treeHeight Empty = 0
treeHeight (Node _ l r) = 1 + max (treeHeight l) (treeHeight r)

-- TODO: from 9).
