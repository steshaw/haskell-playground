{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PostfixOperators #-}

import Steshaw

showK = \n -> "$" ++ show n ++ "k"

double2k :: Double -> String
double2k n = n $> round $> showK

main = putStrLn $ double2k 2000
