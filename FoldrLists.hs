{-
  See:
    http://blog.sigfpe.com/2008/02/purely-functional-recursive-types-in.html

  in trying to understand Empty/Cons from:
    http://apocalisp.wordpress.com/2011/05/30/imperative-vs-functional-programming/
-}

-- {-# LANGUAGE ExistentialQuantification #-}

module FoldrList where

head :: [a] -> a
head = foldr const undefined

-- Inefficient reconstruction of the tail (compared with the usual pattern matching implemention).
tail :: [a] -> [a]
tail xs =
  let Just (_, t) = foldr aux Nothing xs in t where
    aux a Nothing       = Just (a, [])
    aux a (Just (y, z)) = Just (a, y:z)

-- Alternative formulation with foldl - probably not the point. Inefficient use of (++).
tail2 :: [a] -> [a]
tail2 xs = snd $ foldl aux (False, []) xs where
  aux (False, []) _ = (True, [])
  aux (False, _)  _ = error "cannot occur"
  aux (True, ys)  a = (True, ys ++ [a])

-- Lists empty/cons defined as functions.
empty :: a -> b -> b
empty _ b = b

cons :: a -> ((a -> b -> r) -> c -> b) -> (a -> b -> r) -> c -> r
cons h t a b = a h (t a b)

toList xs = xs (:) []

eg1 = toList $ cons 1 $ cons 2 $ empty

-- Idea generalises to other algebraic types.

data Expr = X | Const Int | BinOp (Int -> Int -> Int) Expr Expr

efold :: a -> (Int -> a) -> ((Int -> Int -> Int) -> a -> a -> a) -> Expr -> a
efold x _ _ X = x
efold _ c _ (Const a) = c a
efold x c b (BinOp f lt rt) = b f (efold x c b lt) (efold x c b rt)

eval x = efold x id id
freeX e = efold False (const True) (const (&&)) e
identity e = efold X Const BinOp e
--showE = efold "X" (\n -> "Const " ++ (show n)) (\op el er -> "(BinOp " ++ el ++ " " ++ er ++ ")")

eg2 = eval 3 X
eg3 = eval 3 (Const 2)
eg4 = eval 3 (BinOp (+) X (Const 2))

eg5 = freeX $ BinOp (-) (Const 10) (Const 2)
eg6 = freeX $ BinOp (+) X (Const 2)

eg7 = identity $ BinOp (*) X (Const 10)
eg8 = eval 2 eg7


-- Basically their ends up being a function for every case of the algebraic type that's being encoded.
-- This is like a "handler" for each case.

-- Using a record can make this more obvious:

data E a = E
  { x :: a
  , constant :: Int -> a
  , binOp :: (Int -> Int -> Int) -> a -> a -> a
  }

efold2 :: E a -> Expr -> a
efold2 E {x=x, constant=constant, binOp=binOp} expr = efold x constant binOp expr

showE = efold2 E {
  x = "X"
 ,constant = \n -> "Const" ++ (show n)
 ,binOp = \op l r -> "(BinOp " ++ l ++ " " ++ r ++ ")"
}

eg9 = showE $ BinOp (+) (BinOp (*) X X) (Const 3)
