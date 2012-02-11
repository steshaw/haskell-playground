
data Expr a
  = Var a
  | Val Int
  | Add (Expr a) (Expr a)
  deriving (Show)

instance Monad Expr where
  return x = Var x

  (Var a) >>= f   = f a
  (Val n) >>= f   = Val n
  (Add x y) >>= f = Add (x >>= f) (y >>= f)
{-
  (Add x y) >>= f = do a <- x 
                       b <- y
                       Add (f a) (f b)
-}

e :: Expr Char
e  = Val 10 `Add` Val 20 `Add` Var 'a'
