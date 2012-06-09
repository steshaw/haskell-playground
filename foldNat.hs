{-# LANGUAGE Rank2Types #-}

import Prelude hiding (succ)
--import Prelude ()

data Nat
  = Zero
  | Succ Nat
  deriving (Eq, Read, Show)

foldNat :: forall a. a -> (a -> a) -> Nat -> a
foldNat zero _ (Zero)      = zero
foldNat zero succ (Succ n) = succ (foldNat zero succ n)

plus :: Nat -> Nat -> Nat
plus Zero n = n
plus (Succ k) n = Succ (plus k n)

plus' :: Nat -> Nat -> Nat
plus' m n = foldNat n (\rec -> Succ rec) m
