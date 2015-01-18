data Tree a
  = Empty
    | Node (Tree a) a (Tree a)
  deriving (Show, Eq)

leaf :: a -> Tree a
leaf x = Node Empty x Empty

treeSize :: Tree a -> Integer
treeSize Empty        = 0
treeSize (Node l _ r) = 1 + treeSize l + treeSize r

treeSum :: Tree Integer -> Integer
treeSum Empty        = 0
treeSum (Node l x r) = x + treeSum l + treeSum r

treeDepth :: Tree a -> Integer
treeDepth Empty        = 0
treeDepth (Node l _ r) = 1 + max (treeDepth l) (treeDepth r)

treeFlatten :: Tree a -> [a]
treeFlatten Empty        = []
treeFlatten (Node l a r) = treeFlatten l ++ [a] ++ treeFlatten r

treeFold :: b -> (b -> a -> b -> b) -> Tree a -> b
treeFold e _ Empty = e
treeFold e f (Node l a r) = f (treeFold e f l) a (treeFold e f r)

treeSize_ :: Tree a -> Integer
treeSize_ = treeFold 0 (\l _ r -> 1 + l + r)

treeSum_ :: Tree Integer -> Integer
treeSum_ = treeFold 0 (\ l a r -> l + a + r)

treeDepth_ :: Tree a -> Integer
treeDepth_ = treeFold 0 (\l _ r -> 1 + max l r)

treeFlatten_ :: Tree a -> [a]
treeFlatten_ = treeFold [] (\l a r -> l ++ [a] ++ r)

treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = treeFold minBound (\l a r -> l `max` a `max` r)
