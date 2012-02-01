{-# LANGUAGE BangPatterns #-}

import Criterion.Main
import Data.List (foldl')

leakyMean :: [Double] -> Double
leakyMean xs = sum xs / fromIntegral (length xs)

-- not lazy evaluation friendly
unfriendlyMean :: [Double] -> Double
unfriendlyMean xs = s / fromIntegral l
  where
    (s , l) = foldl' step (0 ,0) xs
    step (s, l) a = (s + a, l + 1)

seqMean :: [Double] -> Double
seqMean xs = s / fromIntegral l
  where
    (s , l) = foldl' step (0 ,0) xs
    step (s , l) a = let s' = s + a
                         l' = l + 1
                     in seq s' (seq l' (s', l'))

bangMean :: [Double] -> Double
bangMean xs = s / fromIntegral l
  where
    (s, l) = foldl' step (0, 0) xs
    step (!s, !l) a = (s + a, l + 1)

data StrictPair a b = SP !a !b

strictPairMean :: [Double] -> Double
strictPairMean xs = s / fromIntegral l
  where
    SP s l = foldl' step (SP 0 0) xs
    step (SP s l) a = SP (s + a) (l + 1)

main = let xs = [1 .. 1 * 1000 * 1000] in defaultMain 
  [ bench "leakyMean"      (whnf leakyMean      xs)
--  , bench "unfriendlyMean" (whnf unfriendlyMean xs)    -- this is so terribly slow
  , bench "seqMean"        (whnf seqMean        xs)
  , bench "bangMean"       (whnf bangMean       xs)
  , bench "strictPairMean" (whnf strictPairMean xs)
  ]
