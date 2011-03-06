{-# LANGUAGE NoMonomorphismRestriction #-}

import Steshaw
import Control.Arrow ((>>>))

showK = \n -> "$" ++ show n ++ "k"

double2k :: Double -> String
double2k = round >>> showK

main = putStrLn $ double2k 2000
