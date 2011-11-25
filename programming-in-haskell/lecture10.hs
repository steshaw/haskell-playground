import Prelude hiding ((++)) -- doesn't seem to work :(

-- Peano numbers
data Nat = Zero | Succ Nat
  deriving (Show)

inf :: Nat
inf = Succ inf

-- very similar in structure to List append.
add           :: Nat -> Nat -> Nat
add Zero     n = n
add (Succ m) n = Succ (m `add` n)

-- comparing with List append (++).

data List a = Empty | Cons a (List a)
  deriving (Show)

(++)               :: List a -> List a -> List a
(++) Empty       ys = ys
(++) (Cons x xs) ys = Cons x (xs ++ ys)

nat2int         :: Nat -> Int
nat2int Zero     = 0
nat2int (Succ n) = 1 + nat2int n

int2nat  :: Int -> Nat
int2nat 0 = Zero
-- the n+k pattern looks elegant here (comparing with nat2int).
--int2nat (n+1) = Succ (int2nat n)
int2nat n = Succ (int2nat (n - 1))
