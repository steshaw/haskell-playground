--
-- Untyped Lambda Calculus
--

import Data.List

type Sym = String

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Expr
  deriving (Eq, Read, Show)

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

[z,s,m,n] = map (Var . (:[])) "zsmn"
app2 f x y = App (App f x) y
zero  = Lam "s" $ Lam "z" z
one   = Lam "s" $ Lam "z" $ App s z
two   = Lam "s" $ Lam "z" $ App s $ App s z
three = Lam "s" $ Lam "z" $ App s $ App s $ App s z
plus  = Lam "m" $ Lam "n" $ Lam "s" $ Lam "z" $ app2 m s (app2 n s z)
