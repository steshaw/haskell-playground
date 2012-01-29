--
-- See http://en.wikipedia.org/wiki/Generalized_algebraic_data_type
--

{-# LANGUAGE GADTs, KindSignatures #-}

data Lam :: * -> * where
  Lift :: a                     -> Lam a
  Tup  :: Lam a -> Lam b        -> Lam (a, b)
  Lam  :: (Lam a -> Lam b)      -> Lam (a -> b)
  App  :: Lam (a -> b) -> Lam a -> Lam b
  Fix  :: Lam (a -> a)          -> Lam a

eval :: Lam t -> t
eval (Lift v)    = v
eval (Tup e1 e2) = (eval e1, eval e2)
eval (Lam f)     = \x -> eval (f (Lift x))
eval (App e1 e2) = (eval e1) (eval e2)
eval (Fix f)     = (eval f) (eval (Fix f))

fact :: Lam (Integer -> Integer)
fact = Fix (Lam (\f -> Lam (\y -> Lift (if eval y == 0 then 1 else eval y * (eval f) (eval y - 1)))))

eg = eval fact 10

main = print eg
