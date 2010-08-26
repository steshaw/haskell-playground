module NumExpr where

data Expr = 
    Atom Integer
  | Add Expr Expr
  | Mul Expr Expr
  deriving (Eq, Show)

instance Num Expr where
  fromInteger i = Atom i
  e1 + e2 = e1 `Add` e2
  e1 * e2 = e1 `Mul` e2

prettyShow :: Expr -> String
prettyShow expr = f False expr
  where
    f _ (Atom n) = show n
    f braceRequired (Add e1 e2) = showExpr braceRequired "+" e1 e2
    f braceRequired (Mul e1 e2) = showExpr braceRequired "*" e1 e2
    showExpr True op e1 e2 = "(" ++ f True e1 ++ op ++ f True e2 ++ ")"
    showExpr False op e1 e2 = f True e1 ++ op ++ f True e2

prop_eg1 = (prettyShow $ 5 + 1 * 3) == "5+(1*3)"
prop_eg2 = (prettyShow $ 5 * 1 + 3) == "(5*1)+3"
