data IntList = Empty | Cons Int IntList
  deriving Show

eg0 :: IntList
eg0 = Cons 1 $ Cons 2 $ Cons 3 $ Cons 4 $ Empty

eg1 :: IntList
eg1 = Cons (-2) $ Cons (-1) $ Cons (0) $ Cons 1 $ Cons 2 $ Cons 3 $ Empty

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

summarisePlus :: IntList -> Int
summarisePlus Empty       = 0
summarisePlus (Cons n ns) = n + (summarisePlus ns)

summariseMul :: IntList -> Int
summariseMul Empty       = 1
summariseMul (Cons n ns) = n * (summariseMul ns)

summarise :: t  -> (Int -> t -> t) -> IntList -> t
summarise empty _    Empty       = empty
summarise empty join (Cons n ns) = join n (summarise empty join ns)

summarisePlus_ :: IntList -> Int
summarisePlus_ = summarise 0 (+)

summariseMul_ :: IntList -> Int
summariseMul_  = summarise 1 (*)
