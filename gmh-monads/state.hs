{-# LANGUAGE TypeSynonymInstances #-}

type State = Int

newtype ST a = S (State -> (a, State))

apply :: ST a -> State -> (a, State)
apply (S f) x = f x

instance Monad ST where
  return x = S (\s -> (x, s))

  st >>= f = S (\s -> let (x, s') = apply st s in apply (f x) s')

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

tree :: Tree Char
tree = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

fresh :: ST Int
fresh = S (\n -> (n, n + 1))

mlabel :: Tree a -> ST (Tree (a, Int))
mlabel (Leaf x)   = do n <- fresh
                       return $ Leaf (x, n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return $ Node l' r'

label :: Tree a -> Tree (a, Int)
label t = fst $ apply (mlabel t) 0

app :: (State -> State) -> ST State
app f = S (\s -> (s, f s))

run :: ST a -> State -> a
run st s = fst $ apply st s

fresh1 :: ST Int
fresh1 = app (+1)

mlabel1 :: Tree a -> ST (Tree (a, Int))
mlabel1 (Leaf x)   = do n <- fresh1
                        return $ Leaf (x, n)
mlabel1 (Node l r) = do l' <- mlabel l
                        r' <- mlabel r
                        return $ Node l' r'

label1 :: Tree a -> Tree (a, Int)
label1 t = run (mlabel1 t) 0
