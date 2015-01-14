{-
infixl 0 |>
(|>) ::  a -> (a -> b) -> b
(|>) = flip ($)
-}

import qualified Data.Set as S
import Data.List

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map twiddle . S.toList $ setNums S.\\ unprimes
  where
    nums :: [Integer]
    nums = [1..n]
    setNums :: S.Set Integer
    setNums = S.fromList nums
    twiddle :: Integer -> Integer
    twiddle m = m * 2 + 1
    unprimes :: S.Set Integer
    unprimes = S.fromList . takeWhile (<= n) . map f . filter (uncurry (<=)) $ cartProd nums nums
      where
        f (i, j) = i + j + (2 * i * j)

sieveSundaram1 :: Integer -> [Integer]
sieveSundaram1 n = map twiddle $ nums \\ unprimes
  where
    nums :: [Integer]
    nums = [1..n]
    twiddle :: Integer -> Integer
    twiddle m = m * 2 + 1
    unprimes :: [Integer]
    unprimes = takeWhile (<= n) . map f . filter (uncurry (<=)) $ cartProd nums nums
      where
        f (i, j) = i + j + (2 * i * j)
