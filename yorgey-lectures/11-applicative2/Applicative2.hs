{-# language Rank2Types #-}
{-# language DeriveFunctor #-}
{-# language InstanceSigs #-}

import Control.Applicative hiding ((*>))
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

printAll :: Show a => [a] -> IO ()
printAll a = a `forM_` print

printEmps1 :: IO ()
printEmps1 = printAll emps1

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

emps2 :: [Employee]
emps2 = getZippy $ Employee <$> Zippy names <*> Zippy phones

printEmps2 :: IO ()
printEmps2 = printAll emps2

--
-- Reader/environment
--

data Big = Big
  { getName         :: Name
  , getSSN          :: String
  , getSalary       :: Integer
  , getPhone        :: Phone
  , getLicensePlate :: String
  , getNumSickDays  :: Int
  }
  deriving (Show)

big1 :: Big
big1 = Big (Name "Brent") "XXX-XX-XXX4" 600000000 (Phone "555-1234") "JGX-55T3" 2

getEmp :: Big -> Employee
getEmp = Employee <$> getName <*> getPhone

emp1 :: Employee
emp1 = getEmp big1

--
-- Applicative API
--

pair :: Applicative f => f a -> f b -> f (a, b)
pair = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (curry snd)

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f = \as -> sequenceA $ map f as

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA = foldr (liftA2 (:)) (pure [])

replicateA :: Applicative f => Int -> f a -> f [a]
replicateA num fas = sequenceA $ replicate num fas
