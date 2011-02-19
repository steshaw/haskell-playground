{-
    Adapted from http://bluebones.net/2007/02/formatting-decimals-in-haskell/
-}
module FormatDecimal (formatDecimal) where

--import Data.Graph.Inductive.Query.Monad (mapFst)
-- This isn't a standard GHC import so implemented below instead.
-- XXX: Looks Arrows 'cause we're pointing at the first element of the tuple.

import Data.List (unfoldr, intersperse)
import Text.Printf

($>) = flip ($)

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (,) (f a) b

formatDecimal decimalPlaces d
  | d < 0.0   = "-" ++ (formatPositiveDecimal (-d))
  | otherwise = formatPositiveDecimal d
  where
    -- Using ($>) makes the following easier to follow (for me at least).
    -- However, it does have the downside of making it more pointy (that is, it is no longer "point free" or pointless).
    formatPositiveDecimal d =
      printf ("%0." ++ (decimalPlaces $> show) ++ "f") d
        $> span (/= '.')
        $> mapFst addCommas
        $> uncurry (++)
    addCommas l =
      reverse l
        $> unfoldr groupBy3
        $> intersperse ","
        $> concat
        $> reverse
    groupBy3 l =
      case splitAt 3 l of 
        ("", "") -> Nothing
        p       -> Just p
