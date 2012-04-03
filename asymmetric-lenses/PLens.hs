module PLens where

import FLens
import qualified Data.Map as M

newtype PLens t c =
  PLens {
    plApply :: t -> Maybe (CoState c t)
  }

leftPLens :: PLens (Either a b) a
leftPLens =
  PLens {
    plApply = \t -> case t of
      Left a  -> Just (CoState a Left)
      Right _ -> Nothing
  }

rightPLens :: PLens (Either a b) b
rightPLens =
  PLens {
    plApply = \t -> case t of
      Right b  -> Just (CoState b Right)
      Left _ -> Nothing
  }

listHeadPLens :: PLens [a] a
listHeadPLens =
  PLens {
    plApply = \t -> case t of
      []    -> Nothing
      h : t -> Just (CoState h (\h -> h : t))
  }

justPLens :: PLens (Maybe a) a
justPLens =
  PLens {
    plApply = \t -> fmap (\z -> CoState z Just) t
  }

-- left-to-right composition
-- plApply :: t -> Maybe (CoState c t)
(>>>) :: PLens t c -> PLens c d -> PLens t d
l1 >>> l2 =
  PLens {
    plApply = \t -> do
      c <- plApply l1 t
      d <- plApply l2 (get c)
      return $ CoState {
                 get = get d
               , set = \x -> set c (set d x)
               }
  }

flens2plens :: FLens a b -> PLens a b
flens2plens l =
  PLens {
    plApply = \a -> Just (apply l a)
  }

--
-- Demo
--

type Addresses = M.Map String Address

data Contact =
  Contact  {
    name :: String
  , addresses :: Addresses
  }
  deriving (Show)

data Address =
  Address {
    street :: String
  , state :: String
  , postcode :: Maybe String
  }
  deriving (Show)

fred = Contact "Fred" $ M.fromList
  [ ("Home", Address "Sunny Cresent" "QLD" (Just "4300"))
  , ("Office", Address "Adelaide Street" "QLD" (Just "4000"))
  ]

addressesL :: FLens Contact Addresses
addressesL = FLens (\p -> CoState
  { get = addresses p
  , set = \a -> p {addresses = a}
  })

postcodeL :: FLens Address (Maybe String)
postcodeL = FLens (\p -> CoState
  { get = postcode p
  , set = \a -> p {postcode = a}
  })

homeAddressL = apply (mapL "Home") (get (apply addressesL fred))

homeAddressPostcodeL = flens2plens ((mapL "Home") `composeL` addressesL) >>> justPLens >>> flens2plens postcodeL

fredsHomePostcode = fmap get $ plApply homeAddressPostcodeL fred -- Just (Just "4300")
setFredsHomePostcode postcode = fmap (flip set postcode) $ plApply homeAddressPostcodeL fred -- Just result

homeAddressPostcodeL' =
  flens2plens ((mapL "Home") `composeL` addressesL)
  >>> justPLens
  >>> flens2plens postcodeL
  >>> justPLens

fredsHomePostcode' = fmap get $ plApply homeAddressPostcodeL' fred -- (Just "4300")
setFredsHomePostcode' postcode = fmap (flip set postcode) $ plApply homeAddressPostcodeL' fred -- Just result
