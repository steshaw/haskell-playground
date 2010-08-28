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
lookupAddress1 = lookupAddress lookupBillingAddress1

test f =
  [f "bill"
  ,f "fred"
  ,f "nobody"
  ,f ""
  ,f "null"
  ,f "pete"
  ]

test1 = test lookupAddress1
test2 = test lookupAddress1
testEq = test1 == test2
