{-# LANGUAGE FlexibleInstances #-}

module C where

import Data.Monoid ((<>))
import Data.Set
import A

data MC = MC deriving (Eq, Ord, Show)

instance Ord b => Ord (T U b MC) where
  compare (T u1 b1 c1) (T u2 b2 c2) = compare u2 u1 <> compare b1 b2 <> compare c1 c2

ins' :: Ord b => T U b MC -> Set (T U b MC) -> Set (T U b MC)
ins' = insert
