{-# LANGUAGE GADTs #-}

data Z = Z
data S n = S n

data Vec n a where
  Nil  :: Vec Z a
  Cons :: a -> Vec n a -> Vec (S n) a

vhead :: Vec (S n) a -> a
vhead (Cons x _) = x

showVec :: Show a => Vec n a -> String
showVec Nil = "Nil"
showVec (Cons x xs) = (show x) ++ "::" ++ (showVec xs)
