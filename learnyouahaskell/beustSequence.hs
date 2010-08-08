--
-- Solution to Cedric Beust's programming challenge.
-- See http://beust.com/weblog/2008/06/27/
--
import Data.List(nub)

f max = filter valid [1..max]
  where valid n = let digits = show n in (length digits) == (length (nub digits))

biggestJump xs = maximum (zipWith (-) (drop 1 xs) (xs))

data Result = Result {maxJump :: Integer, count :: Int} deriving (Show)

also max = let r = f max in 
  Result (biggestJump r) (length r)
