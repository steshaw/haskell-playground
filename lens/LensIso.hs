import Control.Lens

newtype MyInt = MyInt Int

myIntIso :: Iso' Int MyInt
myIntIso = coerced
