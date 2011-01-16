{-# LANGUAGE PostfixOperators #-}

module Main where

import Steshaw
import System (getArgs, getProgName)
import Prelude hiding (catch)
import Control.Exception (try, catch, tryJust)
import Control.Monad (mapM_)
import Control.Monad.Fix (fix)

(%) n = n / 100.0

min_years = 1
max_years = 250
step = 1

weekly_rent = 400.00

usage = do
  progName <- getProgName
  putStrLn ("usage: " ++ progName ++ " <min-interest-rate> <max-interest-rate>\n\n" ++
    "  min-interest-rate   low end of the range of the prevailing risk-free interest rate\n" ++
    "  max-interest-rate   top end of the range of the prevailing risk-free interest rate\n\n" ++
    " e.g. DiscountedCashFlow 3.0 8.0")

main =
--  printDCF (8.0 %) $> last $> snd $> print
  do
  args <- getArgs
  case args of
    [min_rate, max_rate] ->
      case (reads (args !! 0), reads (args !! 1)) :: ([(Double, String)], [(Double, String)]) of
        ([(min_rate, "")], [(max_rate, "")]) -> printTable min_rate max_rate
        otherwise -> usage
    otherwise -> usage

third (a,b,c) = c

dot2 :: Double -> Double
dot2 n = n $> (* 10) $> truncate $> fromIntegral $> (/ 10)

floats min max =
  if min < (max+0.1) then (dot2 min):(floats (min + 0.1) max)
  else []

showResult :: (Double, Integer, Double) -> String
showResult (a, _, c) = (show (dot2 (a*100))) ++ "%: " ++ (float2dollar c)

genTable min_rate max_rate =
  floats min_rate max_rate $> map (%) $> map (\rate -> computeStream rate $> last)

printTable min_rate max_rate =
  genTable min_rate max_rate $> map showResult $> mapM_ putStrLn

printDCF risk_free_interest_rate =
  ([min_years,min_years+step..max_years] // \years -> [1..years]
    // (\year -> weekly_rent * 52 / (1 + risk_free_interest_rate) ^ year)
    $> \ns -> (years, sum ns $> (/ 1000) $> round $> show $> (++ "k")))

computeStream risk_free_interest_rate =
  ([min_years,min_years+step..max_years] // \years -> [1..years]
    // (\year -> weekly_rent * 52 / (1 + risk_free_interest_rate) ^ year)
    $> \ns -> (risk_free_interest_rate, years, sum ns))

computeDCF risk_free_interest_rate = computeStream risk_free_interest_rate $> last $> third

float2dollar :: Double -> String
float2dollar n = n $> round $> \n -> "$" ++ show n ++ "k"

{-
  $> \ns -> (years, sum ns $> (/ 1000) $> round $> \n -> "$" ++ show n ++ "k"))
  $> mapM_ (\(num, value) -> putStrLn ((show num) ++ ": " ++ value))
-}

{-
(10, 200,000.00)
(20, 350,000.00)
(30, 510,000.00)
(40, 519,000.00)
(50, 519,000.00)
-}

--calc :: Float -> Float
--calc years = ([1..years] // (\year -> weekly_rent * 52 / ((1.0 + risk_free_interest_rate) ^ year)) ) $> sum
