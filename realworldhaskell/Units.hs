module Units where

import Data.List (intercalate)

type Units = [String]

data UnitFrac = UnitFrac Double Units
  deriving (Eq)

instance Show UnitFrac where
  show (UnitFrac n units) = show n ++ "_" ++ (intercalate "/" units)

instance Num UnitFrac where
  fromInteger n = UnitFrac (fromInteger n) []
  (UnitFrac n1 u1) + (UnitFrac n2 u2) | u1 == u2  = UnitFrac (n1 + n2) u1
                                      | otherwise = error "Mismatched units in add or subtract"

instance Fractional UnitFrac where
  (UnitFrac n1 u1) / (UnitFrac n2 u2) | u1 == u2  = UnitFrac (n1 / n2) u1
                                      | otherwise = UnitFrac (n1 / n2) (u1 ++ u2)

units n unit = UnitFrac n [unit]

--
-- QuickCheck
--

prop_eg1 = show (units 5 "m" / units 2 "s") == "2.5_m/s"
-- FIXME: How to get QC to check for exception "Mismatched units in add or subtract". Control.Exception.handle?
-- FIXME: Can QC work within IO?
prop_eg2 = units 5 "m" + units 2 "s" == units 7 "?"
prop_eg3 = units 5 "m" + units 2 "m" == units 7 "m"
prop_eg4 = units 5 "m" / 2 == units 2.5 "m"
