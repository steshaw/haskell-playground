--
-- See YesNo type class from http://learnyouahaskell.com/making-our-own-types-and-typeclasses
--

class Trueish a where
  trueish :: a -> Bool

instance Trueish [a] where
  trueish [] = False
  trueish _  = True

instance Trueish Integer where
  trueish 0 = False
  trueish _  = True

data Then = Then
_then = Then
data Else = Else
_else = Else

_if t _then left _else right =
  if trueish t then left else right
