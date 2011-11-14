module Lecture5 where

factors :: Integer -> [Integer]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

factors1 :: Integer -> [Integer]
factors1 n = [ x | x <- [1..n], y <- [1..n], (x * y) == n ]

prime :: Integer -> Bool
prime n = factors n == [1, n]

primes :: [Integer]
primes = [ x | x <- [2..], prime x]

-- adjacent pairs
pairs :: [a] -> [(a, a)]
pairs xs = zip xs (tail xs)

sorted :: Ord a => [a] -> Bool
sorted xs = all (\(a,b) -> a < b) (pairs xs)

adjPairWith :: (a -> a -> b) -> [a] -> [b]
adjPairWith f xs = zipWith f xs (tail xs)

sorted1 :: Ord a => [a] -> Bool
sorted1 xs = and $ adjPairWith (<) xs

-- recursive solution
positions :: Eq a => a -> [a] -> [Integer]
positions a xs = iter 0 xs
  where
    iter n []              = []
    iter n (x:xs) | x == a = n : iter (n+1) xs
    iter n (x:xs) | True   = iter (n+1) xs

-- foldr solution - argh but counts the positions from the right!
positions1 :: Eq a => a -> [a] -> [Integer]
positions1 a xs = snd $ foldr f (0, []) xs
  where
    f e (i, xs) | e == a = (i+1, i : xs)
    f e (i, xs) | True   = (i+1, xs)

zipWithIndex :: [a] -> [(a, Integer)]
zipWithIndex xs = zip xs [0..]

-- using zipWithIndex with comprehension
positions2 :: Eq a => a -> [a] -> [Integer]
positions2 n xs = [ i | (x, i) <- zipWithIndex xs, x == n]

-- using zipWithIndex and map/filter
positions3 :: Eq a => a -> [a] -> [Integer]
positions3 n xs = map snd $ filter ((== n) . fst) $ zipWithIndex xs
