module Tree where
--module Tree (singleton, insert, treeElem) where

import qualified Data.Foldable as F
import Data.Monoid

data Tree a = Empty
            | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton a = Node a Empty Empty

insert x Empty = singleton x
insert x t@(Node a left right)
  | x == a = t
  | x > a = Node a left (insert x right)
  | otherwise = Node a (insert x left) right

treeElem x Empty = False
treeElem x (Node a left right)
  | x == a     = True
  | x < a      = treeElem x left
  | otherwise  = treeElem x right

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node a l r) = F.foldMap f l `mappend` f a `mappend` F.foldMap f r

tree1 = F.foldl (flip insert) Empty [6, 3, 1, 5, 9, 8, 10]
