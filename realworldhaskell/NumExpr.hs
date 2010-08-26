module NumExpr where

import Steshaw
import Data.List (intercalate)
import Test.QuickCheck

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

simplify :: Expr -> Expr
simplify (Mul 1 e) = e
simplify (Mul e 1) = e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify other = other

--
-- QuickChecks
--

instance Arbitrary Expr where
  arbitrary = arbitrary >>= \n -> return $ Atom n

prop_pretty_eg1 = (prettyShow $ 5 + 1 * 3) == "5+(1*3)"
prop_pretty_eg2 = (prettyShow $ 5 * 1 + 3) == "(5*1)+3"
prop_pretty_eg3 = (prettyShow $ simplify $ 5 + 1 * 3) == "5+3"

prop_pretty_1 a@(Atom na) b@(Atom nb) c@(Atom nc) = 
  (prettyShow $ a * b + c) == "(" ++ show na ++ "*" ++ show nb ++ ")+" ++ show nc
prop_pretty_2 a@(Atom na) b@(Atom nb) c@(Atom nc) = 
  (prettyShow $ a + b * c) == show na ++ "+(" ++ show nb ++ "*" ++ show nc ++ ")"

prop_rpn_eg1 = (rpnShow $ 5 + 1 * 3) == "5 1 3 * +"
prop_rpn_eg2 = (rpnShow $ 5 * 1 + 3) == "5 1 * 3 +"
prop_rpn_eg3 = (rpnShow $ simplify $ 5 + 1 * 3) == "5 3 +"
