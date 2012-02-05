-- Interpreter for Imp, based on evaluation semantics
-- Developed for use in COMP3610
-- Clem Baker-Finch

module Interpreter where

import AbsSyn

-- A good match with the semantic definition would be to build the state
-- data structure as a function from Names to Ints:

{-
type State = Name -> Int

arid :: State
arid = \x -> error ("Unknown identifier: " ++ x)

update :: State -> Name -> Int -> State
update s x n = \y -> if x==y then n else s y
-}

-- But we want the interpreter to print out the state which is the result
-- of the interpretation so a "finite" data representation is more
-- appropriate.

-- State is a list of pairs (a graph of the function).  The update
-- operation scans the list for the argument so it is always an O(n)
-- operation.  It would probably be better (and a little easier) to just
-- stick it on the front of the list every time (i.e. a stack).  Straight
-- list manipulation is easier, but I want a new type which can be an
-- instance of Show.

newtype State = State [(Name, Int)]

apply :: State -> Name -> Int

apply (State fm) x =
    case lookup x fm of
    Just n  -> n
    Nothing -> error ("Undeclared identifier: " ++ show x)

update :: State -> Name -> Int -> State

update (State fm) x n =
    State (listUpdate fm x n)
	where listUpdate [] x n = [(x,n)]
	      listUpdate ((y,n'):ps) x n
		  | x==y        = ((x,n):ps)
		  | otherwise   = ((y,n'): listUpdate ps x n)

instance Show State where
--    show :: a -> String
    show (State bindings) = "environment = {\n" ++ concat middle ++ "}\n"
      where
        middle = map fred bindings
        fred (var, value) = "  " ++ var ++ " = " ++ (show value) ++ "\n"
{-
    showsPrec _ (State []) s = s
    showsPrec i (State ((var, n):ps)) s
	= concat [ var, " = ", show n, "\n", showsPrec i (State ps) s]
-}

arid :: State
arid = State []

-- Arithmetic expressions:

eA :: Aexp -> State -> Int

eA (Num n) s      = n
eA (Var x) s      = apply s x
eA (a0 :+: a1) s  = n0 + n1
    where n0 = eA a0 s
          n1 = eA a1 s
eA (a0 :-: a1) s  = n0 - n1
    where n0 = eA a0 s
          n1 = eA a1 s
eA (a0 :*: a1) s  = n0 * n1
    where n0 = eA a0 s
          n1 = eA a1 s

-- Boolean expressions:

eB :: Bexp -> State -> Bool

eB TrueLit s        = True
eB FalseLit s       = False
eB (a0 :=: a1) s    = n0 == n1
    where n0 = eA a0 s
          n1 = eA a1 s
eB (a0 :<=: a1) s   = n0 <= n1
    where n0 = eA a0 s
          n1 = eA a1 s
eB (Not b) s = not t
    where t = eB b s
eB (b0 `And` b1) s  = t0 && t1
    where t0 = eB b0 s
          t1 = eB b1 s
eB (b0 `Or` b1) s   = t0 || t1
    where t0 = eB b0 s
          t1 = eB b1 s

-- Commands:

eC :: Com -> State -> State

eC (x := a) s     = update s x n
    where n = eA a s
eC Skip s         = s
eC (c0 :~: c1) s  = s''
    where s'  = eC c0 s
          s'' = eC c1 s'
eC (If b c0 c1) s
    | t           = s0
    | otherwise   = s1
    where t  = eB b s
          s0 = eC c0 s
          s1 = eC c1 s
eC (While b c) s
    | t           = s''
    | otherwise   = s
    where t   = eB b s
	  s'  = eC c s
          s'' = eC (While b c) s'
