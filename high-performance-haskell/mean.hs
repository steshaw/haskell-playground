{-# LANGUAGE BangPatterns #-}

import Data.List (foldl')

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

main :: IO ()
main = do
  print $ seqMean        xs
  print $ bangMean       xs
  print $ strictPairMean xs
  print $ unfriendlyMean xs
    where xs = [1 .. 10 * 1000 * 1000]
