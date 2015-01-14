
data Tree a
  = Leaf
  | Node Integer (Tree a) a (Tree a)
  deriving (Eq, Show)

foldTree :: [a] -> Tree a
foldTree as = foldr ins Leaf as
  where
    ins :: a -> Tree a -> Tree a
    ins a Leaf = Node 0 Leaf a Leaf
    ins a (Node _ l na r)
      | height l < height r = let
                                lt = ins a l
                                ht = height lt
                              in Node (ht + 1) lt na r -- insert on left
      | otherwise           = let
                                rt = ins a r
                                ht = height rt
                              in Node (ht + 1) l na rt -- insert on right
    height :: Tree a -> Integer
    height Leaf = (-1)
    height (Node h _ _ _) = h

r1 :: Tree Char
r1 =
{-
  Node 3
    (Node 2
      (Node 0 Leaf 'F' Leaf)
      'I'
      (Node 1 (Node 0 Leaf 'B' Leaf) 'C' Leaf))
    'J'
    (Node 2
      (Node 1 (Node 0 Leaf 'A' Leaf) 'G' Leaf)
      'H'
      (Node 1 (Node 0 Leaf 'D' Leaf) 'E' Leaf))
-}
  Node 3
    (Node 2
      (Node 0 Leaf 'C' Leaf)
        'H'
        (Node 1 Leaf 'F' (Node 0 Leaf 'B' Leaf)))
    'J'
    (Node 2
      (Node 1 Leaf 'E' (Node 0 Leaf 'A' Leaf))
      'I'
      (Node 1 Leaf 'G' (Node 0 Leaf 'D' Leaf)))

i1 :: [Char]
i1 = ['A' .. 'J']

t1 :: Bool
t1 = foldTree i1 == r1
