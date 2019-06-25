module M where

headOr :: Foldable t => a -> t a -> a
headOr = foldr const

eg1 :: Int
eg1 = headOr 0 [1 .. 3]

eg2 :: Int
eg2 = headOr 0 []
