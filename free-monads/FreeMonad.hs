--
-- Walking through https://wiki.haskell.org/GHC/Typed_holes
--

module FreeMonad where

data Free f a
  = Pure a
  | Free (f (Free f a))

instance Functor f => Monad (Free f) where
  return a = Pure a

  Pure a >>= f = f a
  Free f >>= g = Free (fmap (>>= g) f)
