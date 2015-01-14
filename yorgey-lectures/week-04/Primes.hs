{-
infixl 0 |>
(|>) ::  a -> (a -> b) -> b
(|>) = flip ($)
-}

import qualified Data.Set as S

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map twiddle . filter notInUnprimes $ nums
  where
    nums :: [Integer]
    nums = [1..n]
    twiddle :: Integer -> Integer
    twiddle m = m * 2 + 1
    notInUnprimes :: Integer -> Bool
    notInUnprimes e = S.notMember e unprimes
    unprimes :: S.Set Integer
    unprimes = S.fromList . takeWhile (<= n) . map f . filter (uncurry (<=)) $ cartProd nums nums
      where
        f (i, j) = i + j + (2 * i * j)
