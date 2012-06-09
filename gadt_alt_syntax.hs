{-# LANGUAGE GADTs #-}

data Exp a
  = (a ~ Int) => LitInt Int
  | (a ~ Bool) => LitBool Bool
  | If (Exp Bool) (Exp Int) (Exp Int)
--  deriving (Show)

bad :: Exp Int
bad = If (LitBool True) (LitInt 42) (LitInt 24)
