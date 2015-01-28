{-# LANGUAGE FlexibleInstances #-}

module B where

import Data.Monoid ((<>))
import Data.Set
import A

data MB = MB deriving (Eq, Ord, Show)

instance Ord c => Ord (T U MB c) where
  compare (T u1 b1 c1) (T u2 b2 c2) = compare u1 u2 <> compare b1 b2 <> compare c1 c2

ins :: Ord c => T U MB c -> Set (T U MB c) -> Set (T U MB c)
ins = insert
