--
-- From slides http://www.cs.nott.ac.uk/~led/talks/led_modularcompilers_slides.pdf
--

import Control.Monad

data Expr
  = Val Int
  | Add Expr Expr
  | Throw
  | Catch Expr Expr
  deriving (Show)

eval :: Expr -> Maybe Int
eval (Val n)     = return n
eval (Add x y)   = do n <- eval x
                      m <- eval y
                      return (n + m)
eval Throw       = mzero
eval (Catch x h) = eval x `mplus` eval h

type Code = [Op]
data Op
  = PUSH Int
  | ADD
  | THROW
  | MARK Code
  | UNMARK
  deriving (Show)

comp :: Expr -> Code
comp (Val n)     = [PUSH n]
comp (Add x y)   = comp x ++ comp y ++ [ADD]
comp Throw       = [THROW]
comp (Catch x h) = [MARK (comp h)] ++ comp x ++ [UNMARK]

exec :: Code -> Int
exec = exec' []

type StackElement = Int
type Stack = [StackElement]

exec' :: Stack -> Code -> Int
exec' [result] []                = result
exec' stack (PUSH n : cs)        = exec' (n : stack) cs
exec' (a : b : stack) (ADD : cs) = exec' (a + b : stack) cs

main = do
  let e = Add (Val 1) (Add (Val 2) (Val 3))
  putStr "expr: "
  print e
  putStr "eval: "
  print $ eval e
  putStr "comp: "
  let code = comp e
  print $ code
  putStr "exec: "
  print $ exec code
