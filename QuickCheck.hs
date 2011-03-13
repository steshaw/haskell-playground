import Data.Char
import Test.QuickCheck

{-
instance Arbitrary Char where
  arbitrary = choose ('\32', '\128')
  coarbitrary c = variant (ord c `rem` 4)
-}

prop_RevApp xs ys =
  reverse (xs ++ ys) == reverse ys ++ reverse xs
    where types = (xs :: [Integer], ys :: [Integer])

prop_RevAppBad xs ys =
  reverse (xs ++ ys) == reverse xs ++ reverse ys
    where types = (xs :: [Integer], ys :: [Integer])

(|>) = flip ($)

ordered :: Ord a => [a] -> Bool
ordered xs = zip xs (tail xs) |> all (uncurry (<=))

ins :: (Ord a) => a -> [a] -> [a]
ins x xs = less ++ x:more
  where
    less = filter (< x) xs
    more = filter (>= x) xs

prop_Insert x xs =
  ordered xs ==> ordered (ins x xs)
    where types = (x :: Integer, xs :: [Integer])
