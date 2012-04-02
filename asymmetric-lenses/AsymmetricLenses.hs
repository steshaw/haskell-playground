{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}

import Control.Arrow ((>>>))
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Control.Monad.State as St

import Person

modifyAge :: (Int -> Int) -> Person -> Person
modifyAge f p = p {age = f (age p)}

modifyAddress :: (Address -> Address) -> Person -> Person
modifyAddress f p = p {address = f (address p)}

getPersonStreet :: Person -> String
getPersonStreet = address >>> street

setPersonStreet :: Person -> String -> Person
setPersonStreet person street = person {address = (address person) {street = street}}

data Lens record field = Lens
  { get :: record -> field
  , set :: (record, field) -> record
  }

ageL :: Lens Person Int
ageL = Lens 
  { get = age
  , set = \(p, a) -> p {age = a}
  }

modify :: Lens record field -> (field -> field) -> record -> record
modify l f p = (set l) (p, (f ((get l) p)))

addressL :: Lens Person Address
addressL = Lens
  { get = address
  , set = \(p, a) -> p {address = a}
  }

streetL :: Lens Address String
streetL = Lens
  { get = street
  , set = \(p, a) -> p {street = a}
  }

stateL :: Lens Address String
stateL = Lens
  { get = state
  , set = \(p, a) -> p {state = a}
  }

composeL :: Lens b c -> Lens a b -> Lens a c
composeL l1 l2 = Lens
  { get = (get l1) . (get l2)
  , set = \(r, f) -> (set l2) (r, (set l1) ((get l2) r, f))
  }

idL :: Lens a a
idL = Lens
  { get = Prelude.id
  , set = \(a, _) -> a
  }

data Category (cat :: * -> * -> *) = Category
  { id :: forall a. cat a a
  , compose :: forall a b c. cat b c -> cat a b -> cat a c
  }

lensCat = Category
  { Main.id = idL
  , compose = composeL
  }

personStreetL = streetL `composeL` addressL
streetOfPerson = get personStreetL person
newPerson = set personStreetL (person, "Elizabeth Street")

foldLens :: [Lens a a] -> Lens a a
foldLens xs = foldr composeL idL xs

--
-- from 5.1 Lens Product Morphism
--
(***) :: Lens r1 f1 -> Lens r2 f2 -> Lens (r1, r2) (f1, f2)
(***) l1 l2 = Lens
  { get = \r -> (get l1 (fst r), get l2 (snd r))
  , set = \(r, f) -> (set l1 ((fst r), (fst f))
                     ,set l2 ((snd r), (snd f))
                     )
  }

eg1 = get (personStreetL *** ageL) (person1, person2)

eg2 = set (personStreetL *** ageL) ((person1, person2), ("Ann Street", 33))

--
-- 5.4 Standard Lens
--

first :: Lens (a, b) a
first = Lens 
  { get = fst
  , set = \(ab, a) -> (a, snd ab)
  }

second :: Lens (a, b) b
second = Lens 
  { get = snd
  , set = \(ab, b) -> (fst ab, b)
  }

mapL :: Ord k => k -> Lens (M.Map k v) (Maybe v)
mapL k = Lens
  { get = \m -> M.lookup k m
  , set = \(m, v) -> case v of
      Nothing -> M.delete k m
      Just w  -> M.insert k w m
  }

m = M.fromList [(1, "one"), (2, "two")]
ml = mapL 3
m1 = get ml m
m2 = set ml (m, Just "three")
m3 = get ml m2

setL :: Ord k => k -> Lens (S.Set k) Bool
setL k = Lens
  { get = \s -> S.member k s
  , set = \(s, p) -> case p of
      False -> S.delete k s
      True  -> S.insert k s
  }

s = S.fromList [2, 4]

--
-- 5.7 Emulating Imperative Programming
--

(+=) :: (Num f) => Lens r f -> f -> St.State r f
l += n = St.state (\r ->
           let w = get l r + n
           in (w, (set l) (r, w)))

(<==) :: Lens r f -> f -> St.State r f
l <== f = St.state (\r -> (f, (set l) (r, f)))

st :: Lens r f -> St.State r f
st l = St.state (\s -> ((get l) s, s))

--
-- Example
--

data Employee =
  Employee {
    emName :: String
  , emSalary :: Integer
  , emAge :: Integer
  }
  deriving (Show, Eq)

emNameL =
  Lens {
    get = emName
  , set = \(e, v) -> e {emName = v}
  }

emSalaryL =
  Lens {
    get = emSalary
  , set = \(e, v) -> e {emSalary = v}
  }

emAgeL =
  Lens {
    get = emAge
  , set = \(e, v) -> e {emAge = v}
  }

modification = do
  _ <- emSalaryL += 100
  n <- st emNameL
  _ <- emNameL <== (n ++ " Jones")
  e <- St.get
  return e

applyModification em = St.evalState modification em

bill = Employee "Bill" 1100 33
bill' = applyModification bill

--
-- Simply alternatives of this example.
--
applyModification2 em = em {emSalary = (emSalary em) + 100, emName = (emName em) ++ " Jones"}
bill2 = applyModification bill

plus100 em = em {emSalary = (emSalary em) + 100}
plusJones em = em {emName = (emName em) ++ " Jones"}
applyModification3 = plus100 . plusJones
bill3 = applyModification3 bill
