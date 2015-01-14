
xor :: [Bool] -> Bool
xor = foldr f False
  where
    f True b = not b
    f False b = b

map_ :: (a -> b) -> [a] -> [b]
map_ f = foldr ((:) . f) []

data Op
  = Minus
  | Plus
  deriving (Show)

data Exp
  = Op Op Exp Exp
  | Lit Integer
  deriving (Show)

t1 = foldr (-) 1 [4, 2] == 4 - (2 - 1)
t2 = foldl (-) 1 [4, 2] == (1 - 4) - 2

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f a bs = foldr fr a bs
  where
    fr :: a -> b -> b
    fr = error "oops"

