{-
    Adapted from http://bluebones.net/2007/02/formatting-decimals-in-haskell/
-}
module FormatDecimal (formatDecimal) where

--import Data.Graph.Inductive.Query.Monad (mapFst)
import List
import Text.Printf

($>) = flip ($)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a,b) = ((f a), b)

formatDecimal decimalPlaces d
    | d < 0.0   = "-" ++ (formatPositiveDecimal (-d))
    | otherwise = formatPositiveDecimal d
    where formatPositiveDecimal = uncurry (++) . mapFst addCommas . span (/= '.') . printf ("%0." ++ decimalPlaces $> show ++ "f")
          addCommas = reverse . concat . intersperse "," . unfoldr splitIntoBlocksOfThree . reverse
          splitIntoBlocksOfThree l = case splitAt 3 l of ([], _) -> Nothing; p-> Just p
