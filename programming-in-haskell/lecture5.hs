
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
