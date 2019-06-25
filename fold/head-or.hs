headOr :: a -> [a] -> a
headOr =
  \a -> \xs ->
    case xs of
      [] -> a
      (h : _) -> h

eg1 :: Int
eg1 = headOr 0 [1 .. 3]

eg2 :: Int
eg2 = headOr 0 []
