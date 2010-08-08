module Cons where

import qualified Data.Foldable as F
import Data.Monoid

data Cons a = Empty | Cons a (Cons a) deriving (Show)

consMap f Empty = Empty
consMap f (Cons a c) = (Cons (f a) (consMap f c))

instance Functor Cons where
  fmap = consMap

consFoldRight f i Empty = i
consFoldRight f i (Cons a rest) = f a (consFoldRight f i rest)

instance F.Foldable Cons where
  foldMap f Empty = mempty
  foldMap f (Cons a r) = f a `mappend` (F.foldMap f r)

{-
toList Empty = []
toList (Cons a cs) = (:) a (toList cs)
-}
--toList = consFoldRight (:) []
toList cs = F.foldr (:) [] cs

fromList xs = F.foldr Cons Empty xs

conses = fromList [1..3]
cs = conses

(>>>) = flip ($)
