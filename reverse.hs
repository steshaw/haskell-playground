--
-- small example derived from Kathleen Fisher's "Algol and Haskell" slides
--   http://www.stanford.edu/class/cs242/slides/AlgolAndHaskell.ppt
--

module Main where

import Prelude hiding (reverse)
import Test.QuickCheck

reverse xs =
  let rev ([], z)   = z
      rev (y:ys, z) = rev (ys, y:z{-accumulate-})
  in rev(xs, [])

type TS = [Int]

prop_revRev :: TS -> Bool
prop_revRev ls = reverse (reverse ls) == ls
