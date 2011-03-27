--
-- From http://www.haskell.org/ghc/docs/latest/html/users_guide/data-type-extensions.html#gadt
--

{-# LANGUAGE GADTs #-}

data Term a where
  Lit    :: { val  :: a }        -> Term a
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

eval :: Term a -> a
eval (Lit x) = x
eval (Succ x) = (eval x) + 1
eval (Pred x) = (eval x) - 1
eval (IsZero x) = (eval x) == 0
eval (Pair a b) = (eval a, eval b)
eval (If p a b) = if (eval p) then (eval a) else (eval b)

e1 :: (Int, Int) -- without declaration is (Integer, Int)
e1 = eval $ Pair (Lit 1) $ Succ $ Succ $ Pred $ Lit 1

e2 :: String
e2 = eval $ If (IsZero (Lit 1)) (Lit "yep") (Lit "nah")
