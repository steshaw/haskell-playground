--
-- From slides http://www.cs.nott.ac.uk/~led/talks/led_modularcompilers_slides.pdf
--
-- With my own interpreter function, `exec`.
-- 

data Expr
  = Val Int
  | Add Expr Expr
  deriving (Show)

eval :: Expr -> Int
eval (Val n)   = n
eval (Add x y) = eval x + eval y

type Code = [Op]
data Op
  = PUSH Int
  | ADD
  deriving (Show)

comp :: Expr -> Code
comp (Val n)   = [PUSH n]
comp (Add x y) = comp x ++ comp y ++ [ADD]

exec :: Code -> Int
exec = exec' []
  where
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
