module Main where

import Data.Foldable as F

toList1 :: Foldable c => c a -> [a]
toList1 = F.foldr (\a -> (a:)) [] -- best?

toList2 :: Foldable c => c a -> [a]
toList2 = F.foldMap (\x -> [x]) -- slow as uses (++)

toList3 :: Foldable c => c a -> [a]
toList3 c = F.foldMap (\a -> (a:)) c [] -- ok, uses (.) but basically traverses twice.
