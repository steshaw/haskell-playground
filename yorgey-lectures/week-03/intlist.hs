
data IntList = Empty | Cons Int IntList
  deriving Show

addOneToAll :: IntList -> IntList
addOneToAll Empty       = Empty
addOneToAll (Cons x xs) = Cons (x + 1) (addOneToAll xs)

absAll :: IntList -> IntList
absAll Empty       = Empty
absAll (Cons x xs) = Cons (abs x) (absAll xs)

squareAll :: IntList -> IntList
squareAll Empty       = Empty
squareAll (Cons x xs) = Cons (x * x) (squareAll xs)

pattern :: (Int -> Int) -> IntList -> IntList
pattern _ Empty       = Empty
pattern f (Cons x xs) = Cons (f x) (pattern f xs)

addOneToAll_ :: IntList -> IntList
addOneToAll_ = pattern (+1)

absAll_ :: IntList -> IntList
absAll_ = pattern abs

squareAll_ :: IntList -> IntList
squareAll_ = pattern (\x -> x * x)
