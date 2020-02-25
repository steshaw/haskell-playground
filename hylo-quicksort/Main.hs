--
-- HT https://bartoszmilewski.com/2020/02/24/math-is-your-insurance-policy/
--

{-# LANGUAGE DeriveFunctor #-}

import Data.List

import Control.Recursion

type Algebra f a = f a -> a
type Coalgebra f a = a -> f a

{-
import Control.Arrow ((>>>))

newtype Term f = In { out :: f (Term f) }

cata :: (Functor f) => Algebra f a -> Term f -> a
cata f = out >>> fmap (cata f) >>> f

ana :: (Functor f) => Coalgebra f a -> a -> Term f
ana f = f >>> fmap (ana f) >>> In

hylo :: (Functor f) => Coalgebra f a -> Algebra f b -> a -> b
hylo f g = ana f >>> cata g
-}

data TreeF a r = Leaf | Node a r r
  deriving Functor

split :: Ord a => Coalgebra (TreeF a) [a]
split [] = Leaf
split (a: as) = Node a l r
  where (l, r) = partition (< a) as

join :: Algebra (TreeF a) [a]
join Leaf = []
join (Node a l r) = l ++ [a] ++ r

qsort :: Ord a => [a] -> [a]
qsort = hylo join split

main = print $ qsort [2, 3, 1]
