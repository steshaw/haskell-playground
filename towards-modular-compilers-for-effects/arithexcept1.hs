--
-- From slides http://www.cs.nott.ac.uk/~led/talks/led_modularcompilers_slides.pdf
--

import Control.Monad
import Debug.Trace

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
  deriving (Eq, Show)

comp :: Expr -> Code
comp (Val n)     = [PUSH n]
comp (Add x y)   = comp x ++ comp y ++ [ADD]
comp Throw       = [THROW]
comp (Catch x h) = [MARK (comp h)] ++ comp x ++ [UNMARK]

exec :: Code -> Maybe Int
exec = exec' []

type StackElement = Int
type Stack = [StackElement]

exec' :: Stack -> Code -> Maybe Int
exec' [n] []                             = Just n
exec' stack (PUSH n : code)              = exec' (n : stack) code
exec' (a : b : stack) (ADD : code)       = exec' ((a + b) : stack) code
exec' stack (MARK h : code)              = let result = exec' stack code in
                                           case result of
                                             Just n -> Just n
                                             Nothing -> exec' stack (h ++ rest)
                                               where
                                                 rest = tail $ dropWhile (/= UNMARK) code
exec' stack (THROW : code)               = Nothing
exec' stack (UNMARK : code)              = exec' stack code

go e = do
  putStr "\nexpr: "
  print e
  putStr "eval: "
  print $ eval e
  putStr "comp: "
  let code = comp e
  print $ code
  putStr "exec: "
  print $ exec code

main = sequence_ 
  [ go $ Val 27 `Add` Val 15 -- example from slide 12
  , go $ Throw `Catch` Throw -- example from slide 12
  , go $ Throw `Catch` (Val 1336 `Add` Val 1) -- example from slide 12
  , go $ Add (Val 1) (Add (Val 2) (Val 3))
  , go $ Add (Val 1) (Catch (Add (Val 2) (Val 3)) (Val 10))
  , go $ Add (Val 1) (Catch (Add (Val 2) (Throw)) (Val 10))
  ]
