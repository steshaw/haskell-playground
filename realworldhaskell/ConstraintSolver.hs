module ConstraintSolver () where

import Steshaw

multiplyTo_1 n =
  [ (x,y) | x <- [1..n], y <- [x..n], x * y == n ]

multiplyTo_2 n = do
  x <- [1..n]
  y <- [x..n]
  if x * y == n then return (x, y) else []

guarded True xs = xs
guarded False _ = []

multiplyTo_3 n = do
  x <- [1..n]
  y <- [x..n]
  guarded (x * y == n) $ return (x, y)
  
