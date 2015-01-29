{-# Language MagicHash #-}

module PlusInt where

import GHC.Base (Int(I#), (+#))

-- from GHC.Base.plusInt
plusInt :: Int -> Int -> Int
(I# x) `plusInt`  (I# y) = I# (x +# y)
