module Class (TrafficLight, Tofu, Frank(Frank)) where

import qualified Data.Map(Map, map)

class Equal a where
  eq :: a -> a -> Bool
  neq :: a -> a -> Bool


data TrafficLight = Red | Yellow | Green
  deriving (Enum)

instance Eq TrafficLight where
  Red == Red = True
  Green == Green = True
  Yellow == Yellow = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "TrafficLight:Red"
  show Yellow = "TrafficLight:Yellow"
  show Green = "TrafficLight:Green"


data Option a = None | Some a

instance Eq a => Eq (Option a) where
  Some a == Some b = a == b
  None == None = True
  _ == _ = False

{-
instance Functor (Data.Map.Map k) where
  fmap f m = Data.Map.map f m
-}

class Tofu t where
  tofu :: j a -> t a j

data Frank a b = Frank {ff :: b a} deriving (Show)

data Barry t k p = Barry {yabba :: p, dabba :: t k} deriving (Show)

instance Functor (Barry a b) where
  fmap f (Barry x y) = Barry (f x) y
