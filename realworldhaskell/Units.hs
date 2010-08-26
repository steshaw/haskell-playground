module Units where

import NumExpr
import Data.List (intercalate)

type Units = [String]

data UnitFrac n = UnitFrac n Units
  deriving (Eq)

instance (Show n) => Show (UnitFrac n) where
  show (UnitFrac n []) = show n
  show (UnitFrac n units) = show n ++ "_" ++ (intercalate "/" units)

instance (Num n) => Num (UnitFrac n) where
  fromInteger n = UnitFrac (fromInteger n) []
  (UnitFrac n1 u1) + (UnitFrac n2 u2) | u1 == u2  = UnitFrac (n1 + n2) u1
                                      | otherwise = error "Mismatched units in add or subtract"
  -- FIXME: Does not handle 10s * 20s = 30s2
  (UnitFrac n1 u1) * (UnitFrac n2 u2) | u1 == u2  = UnitFrac (n1 * n2) u1 -- FIXME
                                      | otherwise = UnitFrac (n1 * n2) (u1 ++ u2)

instance (Fractional n) => Fractional (UnitFrac n) where
  (UnitFrac n1 u1) / (UnitFrac n2 u2) | u1 == u2  = UnitFrac (n1 / n2) []
                                      | otherwise = UnitFrac (n1 / n2) (u1 ++ u2)

--degreesToRadians n = n / (360 / (2 * pi))
degreesToRadians n = 2 * pi * n / 360

instance (Floating n) => Floating (UnitFrac n) where
  -- XXX: What is this "1.0" unit?
  sin (UnitFrac n ["rad"]) = UnitFrac (sin n) []
  sin (UnitFrac n ["deg"]) = sin (UnitFrac (degreesToRadians n) ["rad"])

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
prop_eg5 = show (10 * units 5 "m" / units 2 "s") == "25.0_m/s"
prop_eg6 = show (sin (units (pi / 2) "rad")) == "1.0"
prop_eg7 = show (sin (units 90 "deg")) == "1.0"
prop_eg8 = show ((units 50 "m") * sin (units 90 "deg")) == "50.0_m"
prop_eg9 = show ((units 50 "m" * sin (units 90 "deg")) :: UnitFrac (Expr Double)) == "50.0*sin((2.0*pi)*90.0)/360.0)_m"

eg10 = units 50 "m" * sin (units 90 "deg") :: UnitFrac (Expr Double)
prop_eg10 = show eg10 == "50.0*sin(((2.0*pi)*90.0)/360.0)_m"
