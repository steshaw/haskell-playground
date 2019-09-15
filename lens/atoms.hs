--
-- A walk through Control.Lens.Tutorial
-- 
-- https://hackage.haskell.org/package/lens-tutorial-1.0.4/docs/Control-Lens-Tutorial.html
--

import Control.Lens

newtype MyInt = MyInt Int

myIntIso :: Iso' Int MyInt
myIntIso = coerced

data Atom = Atom {
  _element :: String,
  _point :: Point
} deriving Show

data Point = Point {
  _x :: Double,
  _y :: Double
} deriving Show

data Molecule = Molecule {
  _atoms :: [Atom]
} deriving Show

shiftAtomX :: Atom -> Atom
shiftAtomX (Atom e (Point x y)) = Atom e (Point (x + 1) y)
