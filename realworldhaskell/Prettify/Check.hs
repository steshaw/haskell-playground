module Prettify.Check where

import Prettify
import Test.QuickCheck
import Control.Monad (liftM)
import Control.Applicative ((<$>))
import System.Random (newStdGen, getStdGen)
import Data.List (intersperse)

instance Arbitrary Char where
  arbitrary = elements ['a'..'z']
  coarbitrary = undefined

instance Arbitrary Doc where
  arbitrary = oneof 
    [ return Empty
    , arbitrary >>= return . char
    , arbitrary >>= return . text
    , return Line
    , arbitrary >>= \ doc1 -> arbitrary >>= \ doc2 -> return (Concat doc1 doc2)
    , arbitrary >>= \ doc1 -> arbitrary >>= \ doc2 -> return (Union doc1 doc2)
    ]
  coarbitrary = undefined

prop_empty_id x = 
  empty <> x == x &&
  x <> empty == x

prop_char c = char c == Char c

prop_text s = text s == if null s then Empty else Text s

prop_line = line == Line

prop_double d = double d == text (show d)

prop_hcat xs = hcat xs == glue xs
  where
    glue [] = empty
    glue (d:ds) = d <> glue ds

prop_punctuate s xs = punctuate s xs == combine (intersperse s xs)
  where
    combine []              = []
    combine [x]             = [x]
    combine (x:Empty:ys)    = x:combine ys
    combine (Empty:y:ys)    = y:combine ys
    combine (x:y:ys)        = x <> y : combine ys

