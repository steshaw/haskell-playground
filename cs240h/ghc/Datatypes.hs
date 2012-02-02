module Datatypes where

import Prelude hiding (Nothing, Just, Maybe)
import Data.Int (Int32)

data Maybe a = Nothing | Just a

none = Nothing

someInt = Just (1 :: Int)

someInteger = Just (1 :: Integer)

someInt32 = Just (1 :: Int32)
