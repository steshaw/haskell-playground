{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

--
-- Working through http://www.andres-loeh.de/DGP-Intro.pdf
--

eqList :: (a -> a -> Bool) -> [a] -> [a] -> Bool
eqList _ [] [] = True
eqList eqA (a1 : xs1) (a2 : xs2) = eqA a1 a2 && eqList eqA xs1 xs2
eqList _ _ _ = False

data Tree a = Leaf a | Node (Tree a) (Tree a)
  deriving (Show)

data Rose a = Fork a [Rose a]
  deriving (Show)

eqRose :: (a -> a -> Bool) -> Rose a -> Rose a -> Bool
eqRose eqA (Fork a1 rs1) (Fork a2 rs2) = eqA a1 a2 && eqList (eqRose eqA) rs1 rs2

data Fred = Fred

---

data SnocList a = Lin | SnocList a :> a
  deriving (Show)

-- show List as Cons/Nil
showCons :: (Show a) => [a] -> String
showCons []       = "Nil"
showCons (x : xs) = "(Cons " ++ (show x) ++ " " ++ showCons xs ++ ")"

listToSnocList :: [a] -> SnocList a
listToSnocList []       = Lin
listToSnocList (x : xs) = listToSnocList xs :> x

snocListToList :: SnocList a -> [a]
snocListToList Lin       = [] 
snocListToList (xs :> x) = x : snocListToList xs

---

data U       = U
  deriving (Show)
data a :+: b = L a | R b
  deriving (Show)
data a :*: b = a :*: b
  deriving (Show)

-- data Bool = False | True

type RepBool = U :+: U

class Representable a where
  type Rep a
  from :: a -> Rep a
  to   :: Rep a -> a

instance Representable Bool where
  type Rep Bool = U :+: U
  from False = L U
  from True  = R U
  to   (L U) = False
  to   (R U) = True

instance Representable [a] where
  type Rep [a] = U :+: (a :*: [a])
  from []           = L U
  from (x : xs)     = R (x :*: xs)
  to          (L U) = []
  to (R (x :*: xs)) = x : xs

-- data Tree a = Leaf a | Node (Tree a) (Tree a)
-- data Rose a = Fork a [Rose a]

instance Representable (Tree a) where
  type Rep (Tree a) = a :+: (Tree a :*: Tree a)
  from (Leaf n)    = L n
  from (Node x y)  = R (x :*: y)
  to   (L n)       = Leaf n
  to (R (x :*: y)) = Node x y

instance Representable (Rose a) where
  type Rep (Rose a) = a :*: [Rose a]
  from (Fork x xs) = x :*: xs
  to (x :*: xs)    = Fork x xs

instance Representable Fred where
  type Rep Fred = U
  from Fred = U
  to   U    = Fred

instance Representable Int where
  type Rep Int = Int
  from = id
  to   = id

---

eqSum :: (a -> a -> Bool) -> (b -> b -> Bool) -> a :+: b -> a :+: b -> Bool
eqSum eqA eqB (L a1) (L a2) = eqA a1 a2
eqSum eqA eqB (R b1) (R b2) = eqB b1 b2
eqSum _ _ _ _               = False

eqProd :: (a -> a -> Bool) -> (b -> b -> Bool) -> a :*: b -> a :*: b -> Bool
eqProd eqA eqB (a1 :*: b1) (a2 :*: b2) = eqA a1 a2 && eqB b1 b2

eqUnit :: U -> U -> Bool
eqUnit U U = True

eqInt :: Int -> Int -> Bool
eqInt a b = a == b

---

class GEq a where
  geq :: a -> a -> Bool
instance (GEq a, GEq b) => GEq (a :+: b) where
  geq = eqSum geq geq
instance (GEq a, GEq b) => GEq (a :*: b) where
  geq = eqProd geq geq
instance GEq U where
  geq = eqUnit
instance GEq Int where
  geq = eqInt

--

eq :: (Representable a, GEq (Rep a)) => a -> a -> Bool
eq a b = geq (from a) (from b)

instance GEq Bool where
  geq = eq
instance GEq a => GEq [a] where
  geq = eq
instance GEq a => GEq (Tree a) where
  geq = eq
instance GEq a => GEq (Rose a) where
  geq = eq

