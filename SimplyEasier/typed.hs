--
-- The simply typed lambda calculus.
--

import Data.List
import Control.Monad (when)
import Control.Monad.Error (throwError)

type Sym = String

data Type = Base | Arrow Type Type
  deriving (Eq, Read, Show)

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Type Expr
  deriving (Eq, Read, Show)

{-
whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = foldl App f as

freeVars :: Expr -> [Sym]
freeVars (Var s) = [s]
freeVars (App f a) = freeVars f `union` freeVars a
freeVars (Lam i e) = freeVars e \\ [i]

--
-- Beta reduction
--
subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub e@(Var i) = if i == v then x else e
        sub (App f a) = App (sub f) (sub a)
        sub (Lam i e) =
          if v == i then
            Lam i e
          else if i `elem` fvx then
            let i' = cloneSym e i
                e' = substVar i i' e
            in  Lam i' (sub e')
          else
            Lam i (sub e)
        fvx = freeVars x
        cloneSym e i = loop i
          where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                vars = fvx ++ freeVars e

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v)   (Var v')    = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s e) (Lam s' e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s e) [] = Lam s (nf e)
        spine (Lam s e) (a:as) = spine (subst s a e) as
        spine f as = app f as
        app f as = foldl App f (map nf as)

betaEq :: Expr -> Expr -> Bool
betaEq e1 e2 = alphaEq (nf e1) (nf e2)

{-
  zero ≡ λs.λz.z
  one ≡ λs.λz.s z
  two ≡ λs.λz.s (s z)
  three ≡ λs.λz.s (s (s z))
  plus ≡ λm.λn.λs.λz.m s (n s z)
-}

-}
[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
succType = (Base `Arrow` Base)
natType = succType `Arrow` succType
zero  = Lam "s" succType $ Lam "z" Base z
one   = Lam "s" succType $ Lam "z" Base $ App s z
two   = Lam "s" succType $ Lam "z" Base $ App s $ App s z
three = Lam "s" succType $ Lam "z" Base $ App s $ App s $ App s z
plus  = Lam "m" natType $ Lam "n" natType $ Lam "s" succType $ Lam "z" Base $ app2 m s (app2 n s z)

newtype Env = Env [(Sym, Type)] deriving (Show)

initialEnv :: Env
initialEnv = Env []

extend :: Sym -> Type -> Env -> Env
extend s t (Env r) = Env ((s, t) : r)

type ErrorMsg = String

-- type check "monad"
type TC a = Either ErrorMsg a

findVar :: Env -> Sym -> TC Type
findVar (Env r) s =
  case lookup s r of
    Just t  -> return t
    Nothing -> throwError $ "Cannot find variable " ++ s

-- type check
tCheck :: Env -> Expr -> TC Type
tCheck r (Var s) =
  findVar r s
tCheck r (App f a) = do
  tf <- tCheck r f
  case tf of
    Arrow at rt -> do
      ta <- tCheck r a
      when (ta /= at) $ throwError "Bad function argument type"
      return rt
    _ -> throwError "Non-function in application"
tCheck r (Lam s t e) = do
  let r' = extend s t r
  te <- tCheck r' e
  return $ Arrow t te

-- type check convenience function
typeCheck :: Expr -> Type
typeCheck e =
  case tCheck initialEnv e of
    Left msg -> error $ "Type error: " ++ msg
    Right t  -> t

test_0 = typeCheck zero == natType
test_1 = typeCheck one == natType
test_2 = typeCheck two == natType
test_3 = typeCheck three == natType
test_4 = typeCheck plus == natType `Arrow` (natType `Arrow` natType)
test_5 = typeCheck (app2 plus one zero) == natType
