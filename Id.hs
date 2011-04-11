{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

r :: Integer
r = 4 |> (+1) |> (*2)

-- Idea: (|>) is looks more-or-less like bind (>>=).
{-
class Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
-}
-- Try to produce an instance that "strips" the m.

-- Function compose but left-to-right.
(>.) :: (a -> b) -> (b -> c) -> (a -> c)
(>.) = flip (.)

-- Fudge a return function to make it monadic.
fudge :: Monad m => (a -> b) -> (a -> m b)
fudge f = f >. return

mr :: Monad m => m Integer
mr = return 4 >>= fudge (+1) >>= fudge (*2)

{-
newtype Id a = Id a
  deriving (Show, Eq, Num)

stripId :: Id a -> a
stripId (Id a) = a

instance Monad Id where
  (Id a) >>= f = f a
  return a = Id a
-}

main = mr >>= show >. putStrLn
