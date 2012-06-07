{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}

data Nat 
  = Z
  | S Nat

data Vec :: Nat -> * -> * where
  Nil  :: Vec n a
  Cons :: a -> Vec n a -> Vec (S n) a

vhead :: Vec (S n) a -> a
vhead (Cons x _) = x

showVec :: Show a => Vec n a -> String
showVec Nil = "Nil"
showVec (Cons x xs) = (show x) ++ "::" ++ (showVec xs)
