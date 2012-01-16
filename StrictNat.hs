--
-- Motivation: http://existentialtype.wordpress.com/2011/04/24/the-real-point-of-laziness/
--

data Nat = Zero
         | Succ !Nat
  deriving Show

wrong :: Nat
wrong = Succ undefined

oops :: Nat
oops = Succ wrong

ow :: Nat
ow = Succ ow
