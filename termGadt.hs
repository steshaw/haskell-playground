--
-- From http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt
--

{-# LANGUAGE GADTs #-}

data Term a where
  Lit    :: { val  :: Int }      -> Term Int
  Succ   :: { num  :: Term Int } -> Term Int
  Pred   :: { num  :: Term Int } -> Term Int
  IsZero :: { arg  :: Term Int } -> Term Bool              
  Pair   :: { arg1 :: Term a
            , arg2 :: Term b
            }                    -> Term (a,b)
  If     :: { cnd  :: Term Bool
            , tru  :: Term a
            , fls  :: Term a
            }                    -> Term a
