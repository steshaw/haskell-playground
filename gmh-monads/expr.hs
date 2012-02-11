import Prelude hiding ((>>=))

data Expr 
  = Val Int 
  | Div Expr Expr

eval           :: Expr -> Int
eval (Val n)   = n
eval (Div x y) = eval x `div` eval y

safeDiv   :: Int -> Int -> Maybe Int
safeDiv n m = if m == 0 
              then Nothing
              else Just (n `div` m)

eval2 :: Expr -> Maybe Int
eval2 (Val n) = Just n
eval2 (Div x y) = case eval2 x of
                    Nothing -> Nothing
                    Just n  -> case eval2 y of
                                 Nothing -> Nothing
                                 Just m  -> safeDiv n m

seqn                  :: Maybe a -> Maybe b -> Maybe (a, b)
seqn Nothing _         = Nothing
seqn _       Nothing   = Nothing
seqn (Just x) (Just y) = Just (x, y)

eval3 :: Expr -> Maybe Int
eval3 (Val n) = Just n
eval3 (Div x y) = apply f (eval3 x `seqn` eval3 y)
                  where f (n, m) = safeDiv n m

apply           :: (a -> Maybe b) -> Maybe a -> Maybe b
apply _ Nothing  = Nothing
apply f (Just x) = f x

(>>=)  :: Maybe a -> (a -> Maybe b) -> Maybe b
{-
m >>= f = case m of
            Nothing -> Nothing
            Just x  -> f x
-}
m >>= f = apply f m

data ExprO
  = ValO Int
  | DivO ExprO ExprO
  | Op  ExprO ExprO ExprO

eval3' :: ExprO -> Maybe Int
eval3' (ValO n) = Just n
eval3' (DivO x y) = apply f (eval3' x `seqn` eval3' y)
                    where f (n, m) = safeDiv n m
eval3' (Op x y z) = apply f (eval3' x `seqn` (eval3' y `seqn` eval3' z))
                    where f (a, (b, c)) = Just ((a + b) * c)

eval4 :: Expr -> Maybe Int
eval4 (Val n) = Just n
eval4 (Div x y) = eval4 x >>= \n ->
                  eval4 y >>= \m ->
                  safeDiv n m

-- Can we do eval with apply instead of bind?
eval5          :: Expr -> Maybe Int
eval5 (Val n)   = Just n
eval5 (Div _ _) = undefined
{-
eval5 (Div x y) = apply f (eval3 x `seqn` eval3 y)
                  where f (n, m) = safeDiv n m
-}

eval6 :: Expr -> Maybe Int
eval6 (Val n) = Just n
eval6 (Div x y) = do n <- eval6 x
                     m <- eval6 y
                     safeDiv n m

-- `seqn` using do-notation.
seqn2    :: Maybe a -> Maybe b -> Maybe (a, b)
seqn2 x y = do n <- x
               m <- y
               return (n, m)

-- eval with Op using do notation.
eval7 :: ExprO -> Maybe Int
eval7 (ValO n)    = Just n
eval7 (DivO x y)  = apply f (eval7 x `seqn2` eval7 y)
                    where f (n, m) = safeDiv n m
eval7 (Op x y z)  = do n <- eval7 x
                       m <- eval7 y
                       o <- eval7 z
                       Just ((n + m) * o)

