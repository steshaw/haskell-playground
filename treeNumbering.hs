--
-- Motivating the State Monad for tree numbering.
--
-- See http://www.cs.nott.ac.uk/~nhn/MGS2010/LectureNotes/lecture03.pdf
--

import Control.Monad.State

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show, Eq)

numberTree :: Tree a -> Tree Int
numberTree t = fst (ntAux t 0)
  where
    ntAux :: Tree a -> Int -> (Tree Int, Int)
    ntAux (Leaf _) n = (Leaf n, n + 1)
    ntAux (Node t1 t2) n =
      let (t1', n') = ntAux t1 n
      in let (t2', n'') = ntAux t2 n'
        in (Node t1' t2', n'')

tree :: Tree Char
tree = Node (Leaf 'a') (Node (Leaf 'b') (Leaf 'c'))

type S a = Int -> (a, Int)

sReturn :: a -> S a
sReturn a = \n -> (a, n)

sSeq :: S a -> (a -> S b) -> S b
sSeq sa f = \n -> 
  let (a, n') = sa n
  in (f a n')

sInc :: S Int
sInc = \n -> (n, n + 1)

sNumberTree :: Tree a -> Tree Int
sNumberTree t = fst (ntAux t 0)
  where
    ntAux :: Tree a -> S (Tree Int)
    ntAux (Leaf _) = 
      sInc `sSeq` \n ->
      sReturn (Leaf n)
    ntAux (Node t1 t2) =
      ntAux t1 `sSeq` \t1' ->
      ntAux t2 `sSeq` \t2' ->
      sReturn (Node t1' t2')

smNumberTree :: Tree a -> Tree Int
smNumberTree t = fst (runState (ntAux t) 0)
  where
    ntAux :: Tree a -> State Int (Tree Int)
    ntAux (Leaf _) = do
      n <- get
      put (n + 1)
      return (Leaf n)
    ntAux (Node t1 t2) = do
      t1' <- ntAux t1
      t2' <- ntAux t2
      return (Node t1' t2')
