import Data.Bool
import Control.Monad (liftM2)

data E = One | Two | Three
  deriving (Bounded, Enum, Eq, Show)

-- Total, no exceptions.
next1 :: E -> E
next1 One = Two
next1 Two = Three
next1 Three = One

-- | Using 'succ' which throws.

next2 :: (Bounded a, Eq a, Enum a) => a -> a
next2 e = if e == maxBound then minBound else succ e

next3 :: (Bounded a, Eq a, Enum a) => a -> a
next3 e = bool (succ e) minBound $ e == maxBound

next4 :: (Bounded a, Eq a, Enum a) => a -> a
next4 e = succ e `bool` minBound $ e == maxBound

next5 :: (Bounded a, Eq a, Enum a) => a -> a
next5 = liftM2 (`bool` minBound) succ (== maxBound)

-- https://stackoverflow.com/a/53229752/482382
next6 :: (Bounded a, Eq a, Enum a) => a -> a
next6 = (bool minBound <$> succ) <*> (/= maxBound)

main :: IO ()
main = putStrLn "nothing"
