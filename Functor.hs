{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Functor, map)

class Functor f where
  map :: (a -> b) -> (f a -> f b)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

instance Functor List where
  map :: (a -> b) -> (List a -> List b)
  map _ Nil = Nil
  map f (Cons a as) = Cons (f a) (map f as)

list :: List Integer
list = Cons 1 (Cons 2 (Cons 3 Nil))

list1 :: List Integer
list1 = map (+1) list

newtype Reader r a = Reader (r -> a)

instance Functor (Reader r) where
  map :: (a -> b) -> (Reader r a -> Reader r b)
  map f (Reader g) = Reader (f . g)
