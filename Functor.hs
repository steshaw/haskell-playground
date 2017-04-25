import Prelude hiding (Functor, map)

class Functor f where
  map :: (a -> b) -> (f a -> f b)

data List a
  = Nil
  | Cons a (List a)
  deriving (Show)

instance Functor List where
  map _ Nil = Nil
  map f (Cons a as) = Cons (f a) (map f as)
