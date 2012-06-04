{-# LANGUAGE Rank2Types #-}

import Prelude hiding (succ)
--import Prelude ()

data Nat
  = Zero
  | Succ Nat

foldNat :: forall a. a -> (a -> a) -> Nat -> a
foldNat zero _ (Zero)      = zero
foldNat zero succ (Succ n) = succ (foldNat zero succ n)
