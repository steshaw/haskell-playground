--
-- Generalised Algebraic Data Type
--
-- not Generic Abstract Data Type :)
--
-- From http://hackage.haskell.org/trac/haskell-prime/wiki/GADTs
--

{-# LANGUAGE GADTs, KindSignatures, StandaloneDeriving #-}

data Term :: * -> * where
  Const :: a -> Term a
  Pair  :: Term a -> Term b -> Term (a, b)
  Fst   :: Term (a, b) -> Term a
  Snd   :: Term (a, b) -> Term b
--
-- FIXME: Cannot derive Show :(
-- deriving (Show)
--

-- FIXME: Don't seem to be able to use a standalone deriving declaration either.
--deriving instance Show a => Show (Term a)

eval :: Term a -> a
eval (Const x) = x
eval (Pair t1 t2) = (eval t1, eval t2)
eval (Fst t1) = fst (eval t1)
eval (Snd t1) = snd (eval t1)

t100 = Const 100
e100 = eval t100

tPair = Pair (Const 1) (Const "hi")
ePair = eval tPair

eFst = eval $ Fst $ tPair
eSnd = eval $ Snd $ tPair
