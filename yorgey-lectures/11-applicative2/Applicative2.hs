{-# language Rank2Types #-}
{-# language DeriveFunctor #-}
{-# language InstanceSigs #-}

import Control.Applicative
import Control.Monad (forM_)

newtype Name = Name String deriving (Show)
newtype Phone = Phone String deriving (Show)

data Employee
  = Employee
    { name :: Name
    , phone :: Phone
    }
  deriving (Show)

names :: [Name]
names = Name <$> ["Joe", "Sara", "Mae"]

phones :: [Phone]
phones = Phone <$> ["555-5555", "123-456-7890"]

emps1 :: [Employee]
emps1 = Employee <$> names <*> phones

printEmps1 :: IO ()
printEmps1 = emps1 `forM_` print

type BinOp = Applicative f => f Integer -> f Integer -> f Integer

(.+) :: BinOp
(.+) = liftA2 (+)
(.*) :: BinOp
(.*) = liftA2 (*)

-- nondeterministic arithmetic
n :: [Integer]
n = ([4, 5] .* pure 2) .+ [6, 1]

-- possibly-failing arithmetic
type FailingArith = Maybe Integer
m1 :: FailingArith
m1 = Just 3 .+ Just 5 .* Just 8

m2 :: FailingArith
m2 = ((Just 3) .+ Nothing) .* (Just 8)

newtype Zippy a = Zippy { getZippy :: [a] }
  deriving (Eq, Show, Functor)

-- (+1) [1,2,3]
instance Applicative Zippy where
  pure a = Zippy $ repeat a
  (<*>) :: Zippy (a -> b) -> Zippy a -> Zippy b
  Zippy fs <*> Zippy as = Zippy (zipWith ($) fs as)
