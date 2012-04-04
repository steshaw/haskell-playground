--
-- Lens definition from http://stackoverflow.com/questions/8307370/functional-lenses
--
-- Another (isomorphic) but slightly different representation of a lens.
-- The setter is in curried form and has it's arguments flipped (compared to Lens.hs).
--

data Lens a b = Lens { getter :: a -> b, setter :: b -> a -> a }

composeL :: Lens b c -> Lens a b -> Lens a c
composeL f g = Lens
  { getter = (getter f) . (getter g)
  , setter = \c a -> setter g (setter f c (getter g a)) a
  }
