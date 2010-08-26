module NumExpr where

import Steshaw
import Data.List (intercalate)
import Test.QuickCheck

data Expr = 
    Atom Integer
  | Symbol String
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
    f _ (Symbol s) = s
    f braceRequired (Add e1 e2) = showExpr braceRequired "+" e1 e2
    f braceRequired (Mul e1 e2) = showExpr braceRequired "*" e1 e2
    showExpr True op e1 e2 = "(" ++ f True e1 ++ op ++ f True e2 ++ ")"
    showExpr False op e1 e2 = f True e1 ++ op ++ f True e2

rpnShowExpr op e1 e2 = intercalate " " [rpnShow e1, rpnShow e2, op]

rpnShow :: Expr -> String
rpnShow (Atom n) = show n
rpnShow (Symbol s) = s
rpnShow (Add e1 e2) = rpnShowExpr "+" e1 e2
rpnShow (Mul e1 e2) = rpnShowExpr "*" e1 e2

simplify :: Expr -> Expr
simplify (Mul 1 e) = e
simplify (Mul e 1) = e
simplify (Add e1 e2) = Add (simplify e1) (simplify e2)
simplify other = other

testExpr :: Num a => a
testExpr = 2 * 5 + 3

--
-- QuickChecks
--

instance Arbitrary Expr where
  arbitrary = arbitrary >>= \n -> return $ Atom n

prop_pretty_eg1 = (prettyShow $ 5 + 1 * 3) == "5+(1*3)"
prop_pretty_eg2 = (prettyShow $ 5 * 1 + 3) == "(5*1)+3"
prop_pretty_eg3 = (prettyShow $ simplify $ 5 + 1 * 3) == "5+3"
prop_pretty_eg4 = (prettyShow $ 5 + (Symbol "x") * 3) == "5+(x*3)"

prop_pretty_1 a@(Atom na) b@(Atom nb) c@(Atom nc) = 
  (prettyShow $ a * b + c) == "(" ++ show na ++ "*" ++ show nb ++ ")+" ++ show nc
prop_pretty_2 a@(Atom na) b@(Atom nb) c@(Atom nc) = 
  (prettyShow $ a + b * c) == show na ++ "+(" ++ show nb ++ "*" ++ show nc ++ ")"

prop_rpn_eg1 = (rpnShow $ 5 + 1 * 3) == "5 1 3 * +"
prop_rpn_eg2 = (rpnShow $ 5 * 1 + 3) == "5 1 * 3 +"
prop_rpn_eg3 = (rpnShow $ simplify $ 5 + 1 * 3) == "5 3 +"
prop_rpn_eg4 = (rpnShow $ 5 + (Symbol "x") * 3) == "5 x 3 * +"

prop_test_1 = testExpr == 13
prop_test_2 = rpnShow testExpr == "2 5 * 3 +"
prop_test_3 = prettyShow testExpr == "(2*5)+3"
prop_test_4 = testExpr + 5 == 18
prop_test_5 = prettyShow (testExpr + 5) == "((2*5)+3)+5"
prop_test_6 = rpnShow (testExpr + 5) == "2 5 * 3 + 5 +"
