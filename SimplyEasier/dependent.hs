--
-- The dependently typed lambda calculus.
--

import Data.List
import Control.Monad (liftM, when)
import Control.Monad.Error (throwError)

type Sym = String

data Expr
  = Var Sym
  | App Expr Expr
  | Lam Sym Type Expr
  | Pi  Sym Type Type
  | Kind Kinds
  deriving (Eq, Read, Show)

type Type = Expr

data Kinds
  = Star
  | Box
  deriving (Eq, Read, Show)

freeVars :: Expr -> [Sym]
freeVars (Var s)     = [s]
freeVars (App f a)   = freeVars f `union` freeVars a
freeVars (Lam i t e) = freeVars t `union` (freeVars e \\ [i])
freeVars (Pi  i k t) = freeVars k `union` (freeVars t \\ [i])
freeVars (Kind _)    = []

-- FIXME. othercases? Pi Kinds ?
whnf :: Expr -> Expr
whnf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine f as = foldl App f as

--
-- Beta reduction
--
subst :: Sym -> Expr -> Expr -> Expr
subst v x b = sub b
  where sub (Var i)     = if i == v then x else b
        sub (App f a)   = App (sub f) (sub a)
        sub (Lam i t e) = abstr Lam i t e
        sub (Pi  i t e) = abstr Pi  i t e
        sub (Kind _)    = b
        fvx = freeVars x
        cloneSym e i = loop i
          where loop i' = if i' `elem` vars then loop (i ++ "'") else i'
                vars = fvx ++ freeVars e
        abstr con i t e =
          if v == i then
             con i (sub t) e
          else if i `elem` fvx then
            let i' = cloneSym e i
                e' = substVar i i' e
            in  con i' (sub t) (sub e')
          else
            con i (sub t) (sub e)

substVar :: Sym -> Sym -> Expr -> Expr
substVar s s' e = subst s (Var s') e

-- What about the new cases? Pi, Kinds etc?
alphaEq :: Expr -> Expr -> Bool
alphaEq (Var v)   (Var v')    = v == v'
alphaEq (App f a) (App f' a') = alphaEq f f' && alphaEq a a'
alphaEq (Lam s _ e) (Lam s' _ e') = alphaEq e (substVar s' s e')
alphaEq _ _ = False

nf :: Expr -> Expr
nf ee = spine ee []
  where spine (App f a) as = spine f (a:as)
        spine (Lam s t e) [] = Lam s (nf t) (nf e)
        spine (Lam s _ e) (a:as) = spine (subst s a e) as
        spine (Pi  s k t) as     = app (Pi s (nf k) (nf t)) as
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
base = Var "Base"
arrow = Pi ""
succType = (base `arrow` base)
natType = succType `arrow` succType
zero  = Lam "s" succType $ Lam "z" base z
one   = Lam "s" succType $ Lam "z" base $ App s z
two   = Lam "s" succType $ Lam "z" base $ App s $ App s z
three = Lam "s" succType $ Lam "z" base $ App s $ App s $ App s z
plus  = Lam "m" natType $ Lam "n" natType $ Lam "s" succType $ Lam "z" base $ app2 m s (app2 n s z)

newtype Env = Env [(Sym, Type)] deriving (Show)

initialEnv :: Env
initialEnv = Env [("Base", Var "Base")]

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
  tf <- tCheckRed r f
  case tf of
    Pi x at rt -> do
      ta <- tCheck r a
      when (not (betaEq ta at)) $ throwError $ "Bad function argument type " ++ (show a) ++ " should be " ++ (show at)
      return $ subst x a rt
    _ -> throwError "Non-function in application"
tCheck r (Lam s t e) = do
  _ <- tCheck r t
  let r' = extend s t r
  te <- tCheck r' e
  let lt = Pi s t te
  _ <- tCheck r lt
  return $ lt
tCheck _ (Kind Star) = return $ Kind Box
tCheck _ (Kind Box)  = throwError "Found a Box"
tCheck r (Pi x a b) = do
  s <- tCheckRed r a
  let r' = extend x a r
  t <- tCheckRed r' b
  when ((s, t) `notElem` allowedKinds) $ throwError $ "Bad abstraction: " ++ (show (s, t))
  return t

tCheckRed :: Env -> Expr -> Either ErrorMsg Expr
tCheckRed r e = liftM whnf (tCheck r e)

allowedKinds :: [(Type, Type)]
allowedKinds =
  [(Kind Star, Kind Star)
  ,(Kind Star, Kind Box)
  ,(Kind Box,  Kind Star)
  ,(Kind Box,  Kind Box)]

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
test_4 = typeCheck plus == natType `arrow` (natType `arrow` natType)
test_5 = typeCheck (app2 plus one zero) == natType

--
-- Dependently-typed examples
--
{-
Identity
  id ≡ λa:*.λx:a.x

Pairs
  Pair ≡ λa:*.λb:*.(c:*→((a→b→c)→c))
  pair ≡ λa:*.λb:*.λx:a.λy:b.λc:*.λf:(a→b→c).f x y
  split ≡ λa:*.λb:*.λr:*.λf:(a→b→r).λp:(Pair a b).p r f
  fst ≡ λa:*.λb:*.λp:(Pair a b).split a b a (λx:a.λy:b.x) p
  snd ≡ λa:*.λb:*.λp:(Pair a b).split a b b (λx:a.λy:b.y) p
-}
identity :: Expr
identity = Pi "a" (Kind Star) (Lam "x" (Var "a") (Var "x"))
