module NumExpr where

import Data.List (intercalate)
import Test.QuickCheck.Batch

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

rpnShowExpr op e1 e2 = intercalate " " [rpnShow e1, rpnShow e2, op]

rpnShow :: Expr -> String
rpnShow (Atom n) = show n
rpnShow (Add e1 e2) = rpnShowExpr "+" e1 e2
rpnShow (Mul e1 e2) = rpnShowExpr "*" e1 e2

--
-- QuickChecks
--

prop_pretty_eg1 = (prettyShow $ 5 + 1 * 3) == "5+(1*3)"
prop_pretty_eg2 = (prettyShow $ 5 * 1 + 3) == "(5*1)+3"

prop_rpn_eg1 = (rpnShow $ 5 + 1 * 3) == "5 1 3 * +"
prop_rpn_eg2 = (rpnShow $ 5 * 1 + 3) == "5 1 * 3 +"
