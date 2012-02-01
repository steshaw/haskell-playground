{-# LANGUAGE BangPatterns #-}

import System.Environment
import Data.List (foldl')

leakyMean :: [Double] -> Double
leakyMean xs = sum xs / fromIntegral (length xs)

mean :: [Double] -> Double
mean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (!s, !l) a = (s + a, l + 1)

-- seqMean seems superior in running time and number of collections.
seqMean :: [Double] -> Double
seqMean xs = s / fromIntegral l
  where
    (s , l) = foldl' step (0 ,0) xs
    step (s , l) a = let s' = s + a
                         l' = l + 1
                     in seq s' (seq l' (s', l'))

data StrictPair a b = SP !a !b

strictPairMean :: [Double] -> Double
strictPairMean xs = s / fromIntegral l
  where
    SP s l = foldl' step (SP 0 0) xs
    step (SP s l) a = SP (s + a) (l + 1)

main = do
  [d] <- map read `fmap` getArgs
  print (seqMean [1..d])
