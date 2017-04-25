{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

import Prelude hiding (Functor, map)

-- Here's what goes wrong if you think that `a` and `b` should
-- be parameters to `Functor`.
class Functor a b f where
  map :: (a -> b) -> (f a -> f b)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

instance Functor a b List where
  map :: (a -> b) -> (List a -> List b)
  map _ Nil = Nil
  map f (Cons a as) = Cons (f a) (map f as)

list :: List Integer
list = Cons 1 (Cons 2 (Cons 3 Nil))

list1 :: List Integer
list1 = map (+1) list
