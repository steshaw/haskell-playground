import Prelude hiding (fst, snd)

-----------
-- Pairs --
-----------

type IntPair = (Int, Int)

pair :: Int -> Int -> (Int -> Int -> t) -> t
pair = \a b -> \f -> f a b

p1 = pair 1 2
p2 = pair 3 4

-- accessing both elements of the pair

showPair p = p (\a b -> "(" ++ show a ++ ", " ++ show b ++ ")")

-- access first element
fst p = p (\a _ -> a)

fst' = \a b -> a

-- access second element
snd p = p (\_ b -> b)

snd' = \a b -> b

------------
-- Tuples --
------------

triple :: a -> b -> c -> (a -> b -> c -> t) -> t
triple = \a b c f -> f a b c

showTriple t = t (\a b c -> "(" ++ show a ++ ", " ++ show b ++ ", " ++ show c ++ ")")

first t = t (\a b c -> a)
second t = t (\a b c -> b)
third t = t (\a b c -> c)
