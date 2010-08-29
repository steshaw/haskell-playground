module Monad2 where

import Prelude hiding ((>>=), return)

class (Functor m) => Monad2 m where
  pure :: a -> m a
  bind :: m a -> (a -> m b) -> m b
  xs `bind` f = join $ fmap f xs -- default bind defintion.
  join :: m (m a) -> m a
  join xxs = xxs >>= id -- default join definition.

(>>=) :: Monad2 m => m a -> (a -> m b) -> m b
(>>=) = bind

instance Monad2 [] where
  pure = (:[])
  bind [] _ = []
  bind (x:xs) a2mb = (a2mb x) ++ (bind xs a2mb)
  --join = concat

instance Monad2 Maybe where
  pure = Just
  bind (Just a) a2mb = a2mb a
  bind Nothing  a2mb = Nothing
  --join (Just a) = a
  --join Nothing = Nothing
