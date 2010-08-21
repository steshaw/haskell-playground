{-# LANGUAGE DeriveFunctor, FlexibleInstances #-}

data Tree a
    = Node (Tree a) (Tree a)
    | Leaf a
  deriving (Show)

treeMap :: (a -> b) -> Tree a -> Tree b
treeMap f (Leaf a) = Leaf (f a)
treeMap f (Node l r) = Node (treeMap f l) (treeMap f r)

instance Functor Tree where
  fmap = treeMap

tree1 = Node (Leaf "foo") (Node (Leaf "quux") (Leaf "x"))

data Foo a = Foo a 
  deriving (Show, Functor)

data Eq a => Bar a = Bar a
  deriving (Show)

barMap f (Bar a) = Bar (f a)

{-
data Functor Bar where
  fmap = barMap
-}

instance Functor (Either a) where
  fmap f (Right r) = Right (f r)
  fmap f (Left n) = Left n
