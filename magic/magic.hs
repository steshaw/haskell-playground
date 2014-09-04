-- 
-- Pedagogical experiment in datatype-generic programming (without doing any datatype-generic programming).
--
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

data EnumA = A
  deriving (Show)

data EnumB = B1 | B2
  deriving (Show)

data EnumC = C1 | C2 | C3
  deriving (Show)

data Colour = Red | Blue | Green
  deriving (Show)

data Option a = None | Some a
  deriving (Show)

newtype Show' a = Show' (a -> String)

gs :: Show' a -> (a -> String)
gs (Show' sa) = sa

stringShow :: Show' String
stringShow = Show' id

anyShow :: Show a => Show' a
anyShow = Show' (\a -> show a)

data Error = Error
 
contramap :: (a -> b) -> Show' b -> Show' a
contramap f (Show' sb) = Show' x
  where x = sb . f

unitShow :: Show' ()
unitShow = Show' $ const "Unit"

boolShow :: Show' Bool
boolShow = Show' (\b -> if b then "True" else "False")

intShow :: Show' Int
intShow = Show' (\n -> show n)

pairShow :: forall a b. Show' a -> Show' b -> Show' (a, b)
pairShow (Show' sa) (Show' sb) = Show' x
  where
    x :: (a, b) -> String
    x (a, b) = "(" ++ sa a ++ ", " ++ sb b ++ ")"

eitherShow :: forall a b. Show' a -> Show' b -> Show' (Either a b)
eitherShow (Show' sa) (Show' sb) = Show' x
  where
     x :: Either a b -> String
     x (Left  a) = sa a
     x (Right b) = sb b

type ColourE = Either Bool ()

encodeColour :: Colour -> ColourE
encodeColour Red   = Right ()
encodeColour Blue  = Left False
encodeColour Green = Left True

colorEShow :: Show' ColourE
colorEShow = eitherShow boolShow unitShow

colourShow :: Show' Colour
colourShow = Show' (\colour -> (gs colorEShow) (encodeColour colour))

-----------------------------------------------------------------------
type OptionE a = Either a ()

encodeOption :: forall a. Option a -> OptionE a
encodeOption None     = Right ()
encodeOption (Some a) = Left a

optionEShow :: Show' a -> Show' (OptionE a)
optionEShow sa = eitherShow sa unitShow

optionShow :: Show' a -> Show' (Option a)
optionShow sa = Show' (\opt -> (gs (optionEShow sa)) (encodeOption opt))

colourS :: Colour -> String
colourS = gs colourShow

optionS :: Show' a -> (Option a -> String)
optionS sa = gs (optionShow sa)

-----------------------------------------------------------------------

type EnumAE = ()

encodeEnumA :: EnumA -> EnumAE
encodeEnumA = const ()

enumAEShow :: Show' EnumAE
enumAEShow = unitShow

enumAShow :: Show' EnumA
enumAShow = Show' (\enumA -> (gs (enumAEShow)) (encodeEnumA enumA))

showEnumA :: EnumA -> String
showEnumA = gs enumAShow

-----------------------------------------------------------------------

-----------------------------------------------------------------------

--prettyColour = [(Red, "Red"), (Blue, "Blue"), (Green, "Green")]

--newtype ZipperList a = Z [a] a [a]

-----------------------------------------------------------------------

printEm s1 f1 s2 f2 v = do
  putStrLn $ s1 ++ (f1 v)
  putStrLn $ s2 ++ (f2 v)

main :: IO ()
main = do
  putStrLn "EnumA"
  mapM_ (printEm "show  ==>> " show 
                 "magic ==>> " showEnumA) [A]

  putStrLn "Colours"
  mapM_ (printEm "show  ==>> " show 
                 "magic ==>> " colourS) [Red, Blue, Green]

  putStrLn "\nOptions"
  mapM_ (printEm "show  ==>> " show 
                 "magic ==>> " (optionS intShow)) [None, Some 1, Some 2]
  mapM_ (printEm "show  ==>> " show 
                 "magic ==>> " (optionS stringShow)) [None, Some "Hi", Some "There"]
  mapM_ (printEm "show  ==>> " show 
                 "magic ==>> " (optionS colourShow)) $ [None] ++ (map Some [Red, Blue, Green])
