--
-- Posted to haskell-cafe by Peter Padawitz.
--

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

import Data.Tree (Tree(..))

data Type a where Tree    :: Type a -> Type (Tree a)
                  Int     :: Type Int
                  String  :: Type String

type Traversal1 = forall a.a -> Type a -> a

type Traversal2 = forall a.Type a -> a -> a

tr1 :: Traversal1
tr1 (Node _ (t:_)) (Tree Int) = Node 1 [t]
tr1 n Int                     = n+n
tr1 s String                  = s++s

{-
tr2 :: Traversal2
tr2 (Tree Int) (Node _ (t:_)) = Node 1 [t]
tr2 Int n                     = n+n
tr2 String s                  = s++s
-}

main = putStrLn "hi"
