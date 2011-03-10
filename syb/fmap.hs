--
-- Example from http://www.haskell.org/haskellwiki/Scrap_your_boilerplate
--

{-# LANGUAGE RankNTypes, ScopedTypeVariables, DeriveDataTypeable, FlexibleContexts #-}

import Data.Generics
import Unsafe.Coerce

{-
   | C tags the type that is actually parameterized, so to avoid touching the
   | Int when a ~ Int:
   |
   | > data T a = T Int a
   | 
   | by changing the type (not representation) to:
   | 
   | > x :: T Int (C Int)
-}
newtype C a = C a
  deriving (Data, Typeable)

fmapData :: forall t a b. (Typeable a, Data (t (C a)), Data (t a)) => (a -> b) -> t a -> t b
fmapData f input = 
  uc . everywhere (mkT $ \(x::C a) -> uc (f (uc x))) $ (uc input :: t (C a))
    where uc = unsafeCoerce

data T a = T a a
  deriving (Show)

-- FIXME: Does not compile:
{-
fmap.hs:32:6:
    No instances for (Data (T Int), Data (T (C Int)))
      arising from a use of `fmapData' at fmap.hs:32:6-35
    Possible fix:
      add an instance declaration for (Data (T Int), Data (T (C Int)))
    In the expression: fmapData succ (T 1 1 :: T Int)
    In the definition of `eg1': eg1 = fmapData succ (T 1 1 :: T Int)
-}
--eg1 = fmapData succ (T 1 1 :: T Int)

eg2 = fmapData succ [1,2,3::Int]

eg3 = fmapData succ ([1,2,3] :: [Int])
