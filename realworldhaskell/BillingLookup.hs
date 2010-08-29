import qualified Data.Map as M

import Control.Monad (liftM)
import Steshaw

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier
  = HonestBob
  | MorrisasMobiles
  | PlurocraticPhones
  deriving (Eq, Ord)

type NameToNumber = M.Map PersonName PhoneNumber
type NumberToCarrier = M.Map PhoneNumber MobileCarrier
type CarrierToAddress = M.Map MobileCarrier BillingAddress

type FindCarrierBillingAddress
  = PersonName -> NameToNumber -> NumberToCarrier -> CarrierToAddress -> Maybe BillingAddress

lookupBillingAddress1 :: FindCarrierBillingAddress
lookupBillingAddress1 person phoneMap carrierMap addressMap =
  case M.lookup person phoneMap of
    Nothing -> Nothing
    Just number ->
      case M.lookup number carrierMap of
        Nothing -> Nothing
        Just carrier -> M.lookup carrier addressMap

lookupBillingAddress2 :: FindCarrierBillingAddress
lookupBillingAddress2 person phoneMap carrierMap addressMap =
  M.lookup person phoneMap >>= \ phoneNumber ->
    M.lookup phoneNumber carrierMap >>= \ carrier ->
      M.lookup carrier addressMap

lookupBillingAddress3 :: FindCarrierBillingAddress
lookupBillingAddress3 person phoneMap carrierMap addressMap = do
  phoneNumber <- M.lookup person phoneMap
  carrier <- M.lookup phoneNumber carrierMap
  address <- M.lookup carrier addressMap
  return address

lookupBillingAddress4 :: FindCarrierBillingAddress
lookupBillingAddress4 person phoneMap carrierMap addressMap = do
  phoneNumber <- M.lookup person phoneMap
  carrier <- M.lookup phoneNumber carrierMap
  M.lookup carrier addressMap

-- { Trying to work out why flookup doesn't work (without an explicit type signature
flookup :: (Ord k) => M.Map k a -> k -> Maybe a
flookup = flip M.lookup

expr1 phoneMap = flookup (phoneMap::NameToNumber)
expr2 carrierMap = flookup (carrierMap::NumberToCarrier)
expr3 addressMap = flookup (addressMap::CarrierToAddress)

expr1' phoneMap = flookup phoneMap
expr2' carrierMap = flookup carrierMap
expr3' addressMap = flookup addressMap
-- }

lookupBillingAddress5 :: FindCarrierBillingAddress
lookupBillingAddress5 person phoneMap carrierMap addressMap = do
    phoneNumber <- l phoneMap person
    carrier     <- l carrierMap phoneNumber
    address     <- l addressMap carrier
    return address
  where
    l :: (Ord k) => M.Map k a -> k -> Maybe a -- FIXME: Why is this type def necessary here?
    l = flip M.lookup -- FIXME: somehow 'l' gets the type of the first usage of 'l'.
                      -- FIXME: This works for looking up in the carrierMap only because PersonName and
                      -- FIXME: PhoneNumber are the same type (String).

lookupBillingAddress6 :: FindCarrierBillingAddress
lookupBillingAddress6 person phoneMap carrierMap addressMap =
    flookup phoneMap person >>= flookup carrierMap >>= flookup addressMap

lookupBillingAddress7 :: FindCarrierBillingAddress
lookupBillingAddress7 person phoneMap carrierMap addressMap =
    unit person >>= flookup phoneMap >>= flookup carrierMap >>= flookup addressMap

phoneMap = M.fromList
  [("bill",   "04150000")
  ,("fred",   "04220000")
  ,("nobody", "04210000")
  ,("pete",   "04230000")
  ]
carrierMap = M.fromList
  [("04110000", HonestBob)
  ,("04150000", HonestBob)
  ,("04220000", MorrisasMobiles)
  ,("04230000", PlurocraticPhones)
  ]
addressMap = M.fromList
  [(HonestBob, "1 Smith St, Honest Suburb, Honest Land")
  ,(MorrisasMobiles, "53 High St, Boston, UK")
  ]

lookupAddress f name = f name phoneMap carrierMap addressMap

lookups = map lookupAddress 
  [lookupBillingAddress1
  ,lookupBillingAddress2
  ,lookupBillingAddress3
  ,lookupBillingAddress4
  ,lookupBillingAddress5
  ,lookupBillingAddress6
  ,lookupBillingAddress7
  ]

test f = map f ["bill" , "fred" , "nobody" , "" , "null", "pete"]
tests = map test lookups
testAll = (,) (head tests) (map (== head tests) (tail tests))
printAll = mapM_ putStrLn $ zipWith (\ x y -> show x ++ ": " ++ show y) [1..] tests
