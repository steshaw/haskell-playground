-- foldr  : (a -> b -> b) -> b -> [a] -> b
-- foldr1 : (a -> a -> a) -> [a] -> a

headOr :: Foldable t => a -> t a -> a
headOr b = flip foldr b $
  \a -> const a

eg1 :: Int
eg1 = headOr 0 [1 .. 3]

eg2 :: Int
eg2 = headOr 0 []
