-- Example from http://www.haskell.org/haskellwiki/Scrap_your_boilerplate

{-# LANGUAGE DeriveDataTypeable #-}

import Data.Generics
import Data.Char (isUpper)
 
-- A version of Data.Generics.listify which doesn't recurse into sublists of type [b]
listifyWholeLists :: Typeable b => ([b] -> Bool) -> GenericQ [[b]]
listifyWholeLists blp = flip (synthesize id (.) (mkQ id (\bl _ -> if blp bl then (bl:) else id))) []

anyUpper = any isUpper

data T a = T a
 deriving (Data, Typeable)


ex1 = listify anyUpper "Hi Steve!"
a1 = ex1 == ["Hi Steve!","i Steve!"," Steve!","Steve!"]

ex2 = listify anyUpper ["Hi Steve!"]
a2 = ex2 == ["Hi Steve!","i Steve!"," Steve!","Steve!"]

ex3 = listify anyUpper (T ["Hi Steve!"])
a3 = ex3 == ["Hi Steve!","i Steve!"," Steve!","Steve!"]

ex4 = listify anyUpper ("hello", ([[(T ["Hi Steve!"])]], 1::Int))
a4 = ex4 == ["Hi Steve!","i Steve!"," Steve!","Steve!"]


wex1 = listifyWholeLists anyUpper "Hi Steve!"
wa1 = wex1 == ["Hi Steve!"]

wex2 = listifyWholeLists anyUpper ["Hi Steve!"]
wa2 = wex2 == ["Hi Steve!"]

wex3 = listifyWholeLists anyUpper (T ["Hi Steve!"])
wa3 = wex3 == ["Hi Steve!"]

wex4 = listifyWholeLists anyUpper ("hello", ([[(T ["Hi Steve!"])]], 1::Int))
wa4 = wex4 == ["Hi Steve!"]
