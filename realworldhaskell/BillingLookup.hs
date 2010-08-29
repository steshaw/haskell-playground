import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier 
  = HonestBob 
  | MorrisasMobiles 
  | PlurocraticPhones 
  deriving (Eq, Ord)

type FindCarrierBillingAddress
  = PersonName
  -> M.Map PersonName PhoneNumber
  -> M.Map PhoneNumber MobileCarrier
  -> M.Map MobileCarrier BillingAddress
  -> Maybe BillingAddress

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

lookupBillingAddress5 :: FindCarrierBillingAddress
lookupBillingAddress5 = lookupBillingAddress4
{-
lookupBillingAddress5 person phoneMap carrierMap addressMap = do
  let l = flip M.lookup
  phoneNumber <- l phoneMap person
  carrier     <- l carrierMap phoneNumber
  address     <- l addressMap carrier
  return address
-}

lookupBillingAddress6 :: FindCarrierBillingAddress
lookupBillingAddress6 = lookupBillingAddress4
{-
lookupBillingAddress6 person phoneMap carrierMap addressMap =
    lookup phoneMap person >>= lookup carrierMap >>= lookup addressMap
  where 
    lookup = flip M.lookup
-}

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
  ]

test f = map f ["bill" , "fred" , "nobody" , "" , "null", "pete"]

tests = map test lookups

testAll = (,) (head tests) (map (== head tests) (tail tests))
