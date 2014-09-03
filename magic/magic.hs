{-# LANGUAGE ScopedTypeVariables #-}

module Magic where

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

type OptionE a = Either a ()

encodeOption :: forall a. Option a -> OptionE a
encodeOption None     = Right ()
encodeOption (Some a) = Left a

optionEShow :: Show' a -> Show' (OptionE a)
optionEShow sa = eitherShow sa unitShow

optionShow :: Show' a -> Show' (Option a)
optionShow sa = Show' (\opt -> (gs (optionEShow sa)) (encodeOption opt))

--prettyColour = [(Red, "Red"), (Blue, "Blue"), (Green, "Green")]

--newtype ZipperList a = Z [a] a [a]

colourS :: Colour -> String
colourS = gs colourShow

--optionShow :: forall a. Show a => Show' (Option a)

optionS :: Show' a -> (Option a -> String)
optionS sa = (gs (optionShow sa))


printEm s1 f1 s2 f2 v = do
  putStrLn $ s1 ++ (f1 v)
  putStrLn $ s2 ++ (f2 v)

main :: IO ()
main = do
  -- mapM_ (printEm show colourS) [Red, Blue, Green]

  putStrLn $ show               $ (None :: Option ())
  putStrLn $ optionS unitShow   $ (None :: Option ())
  putStrLn $ show               $ Some 1
  putStrLn $ optionS intShow    $ Some 1
  putStrLn $ show               $ Some "Hi"
  putStrLn $ optionS stringShow $ Some "Hi"
  putStrLn $ show               $ Some Red
  putStrLn $ optionS colourShow $ Some Red
