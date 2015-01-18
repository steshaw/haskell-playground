fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a = Cons a (Stream a)

instance Show a => Show (Stream a) where
  show = show . take 20 . streamToList

streamToList :: Stream a -> [a]
streamToList (Cons a as) = a : streamToList as

streamRepeat :: a -> Stream a
streamRepeat a = Cons a (streamRepeat a)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons a as) = Cons (f a) (streamMap f as)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f initial = Cons initial (streamFromSeed f (f initial))

nats :: Stream Integer
nats = streamFromSeed (+1) 0

streamTail :: Stream a -> Stream a
streamTail (Cons _ as) = as

streamHead :: Stream a -> a
streamHead (Cons a _) = a

ruler :: Stream Integer
ruler = streamMap largestPower2DividesN (streamTail nats)
  where
    largestPower2DividesN a = head $ reverse $ map fst $ filter f (takeWhile ((<=a) . snd) (streamToList powers2))
      where
        f (_, n) = a `mod` n == 0

streamFilter :: (a -> Bool) -> Stream a -> Stream a
streamFilter f (Cons a as)
  | f a                    = Cons a rest
  | otherwise              = rest
    where
      rest = streamFilter f as

powers2 :: Stream (Integer, Integer)
powers2 = streamFromSeed f (0, 1)
  where
    f (i, n) = (i + 1, n * 2)
