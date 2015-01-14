
data IntList = Empty | Cons Int IntList
  deriving Show

eg :: IntList
eg = Cons (-2) $ Cons (-1) $ Cons (0) $ Cons 1 $ Cons 2 $ Empty

addOneToAll :: IntList -> IntList
addOneToAll Empty       = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

each :: (Int -> Int) -> IntList -> IntList
each _ Empty       = Empty
each f (Cons x xs) = Cons (f x) (each f xs)

addOneToAll_ :: IntList -> IntList
addOneToAll_ = each (+1)

absAll_ :: IntList -> IntList
absAll_ = each abs

squareAll_ :: IntList -> IntList
squareAll_ = each (\x -> x * x)

keepOnlyEven :: IntList -> IntList
keepOnlyEven Empty = Empty
keepOnlyEven (Cons x xs)
  | even x    = Cons x (keepOnlyEven xs)
  | otherwise = keepOnlyEven xs

keepLessThan2 :: IntList -> IntList
keepLessThan2 Empty = Empty
keepLessThan2 (Cons x xs)
  | x < 2    = Cons x (keepLessThan2 xs)
  | otherwise = keepLessThan2 xs

keep :: (Int -> Bool) -> IntList -> IntList
keep _ Empty = Empty
keep f (Cons x xs)
  | f x    = Cons x (keep f xs)
  | otherwise = keep f xs

keepOnlyEven_ :: IntList -> IntList
keepOnlyEven_ = keep even

keepLessThan2_ :: IntList -> IntList
keepLessThan2_ = keep (< 2)
