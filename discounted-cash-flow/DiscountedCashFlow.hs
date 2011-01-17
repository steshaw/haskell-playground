{-# LANGUAGE PostfixOperators #-}

module Main where

import Steshaw
import System (getArgs, getProgName)
import Prelude hiding (catch, length)
import Data.List (intersperse)
import Control.Exception (try, catch, tryJust)
import Control.Monad (mapM_, join)
import Control.Monad.Fix (fix)

(%) n = n / 100.0

min_years = 1
max_years = 250
step = 1

weeks_per_year = 52

usage = do
  progName <- getProgName
  putStrLn ("usage: " ++ progName ++ " weekly-rent <min-interest-rate> <max-interest-rate>\n\n" ++
    "  weekly-rent         weekly rent in dollars                                          \n" ++
    "  min-interest-rate   low end of the range of the prevailing risk-free interest rate\n" ++
    "  max-interest-rate   top end of the range of the prevailing risk-free interest rate\n\n" ++
    " e.g. DiscountedCashFlow 3.0 8.0")

print_dcf_at_8 = printDCF 500.00 (8.0 %) $> last $> snd $> print

main =
  do
  args <- getArgs
  case args of
    [weekly_rent, min_rate, max_rate] ->
      case (reads weekly_rent, reads min_rate, reads max_rate) :: ([(Double, String)], [(Double, String)], [(Double, String)]) of
        ([(weekly_rent, "")], [(min_rate, "")], [(max_rate, "")]) -> printTable weekly_rent min_rate max_rate
        otherwise -> usage
    otherwise -> usage

third (a,b,c) = c

dot2 :: Double -> Double
dot2 n = n $> (* 10) $> truncate $> fromIntegral $> (/ 10)

floats min max =
  if min < (max+0.1) then (dot2 min):(floats (min + 0.1) max)
  else []

show_multiple n = (show (truncate n)) ++ "x"

showResult :: (Double, Integer, Double, Double, Double) -> String
showResult (interestRate, _, price, xweekly, xannual) = 
  (fill 4 (show (dot2 (interestRate*100)))) ++ "%  " ++ (fill 10 (double2dollar price)) ++ " " ++ (fill 7 (show_multiple xweekly)) ++ " " ++ (fill 7 (show_multiple xannual))

calculateMultiples weekly_rent (interestRate, years, price) = 
  (interestRate, years, price, price / weekly_rent, price / (weekly_rent * weeks_per_year))

genTable weekly_rent min_rate max_rate =
  floats min_rate max_rate $> map (%) $> map (\rate -> computeStream weekly_rent rate $> last $> calculateMultiples weekly_rent)

printTable weekly_rent min_rate max_rate =
  genTable weekly_rent min_rate max_rate $> map showResult $> mapM_ putStrLn

printDCF weekly_rent risk_free_interest_rate =
  ([min_years,min_years+step..max_years] // \years -> [1..years]
    // (\year -> weekly_rent * 52 / (1 + risk_free_interest_rate) ^ year)
    $> \ns -> (years, sum ns $> (/ 1000) $> round $> show $> (++ "k")))

computeStream weekly_rent risk_free_interest_rate =
  ([min_years,min_years+step..max_years] // \years -> [1..years]
    // (\year -> weekly_rent * 52 / (1 + risk_free_interest_rate) ^ year)
    $> \ns -> (risk_free_interest_rate, years, sum ns))

computeDCF weekly_rent risk_free_interest_rate = computeStream weekly_rent risk_free_interest_rate $> last $> third

separate1000s' :: Integer -> [Integer]-> [Integer]
separate1000s' n ns =
  if (n `div` 1000) > 0 then separate1000s' (n `div` 1000) ((n `mod` 1000):ns)
  else n:ns

separate1000s :: Integer -> [Integer]
separate1000s n = separate1000s' n []

separate1000s_on_double :: Double -> [Integer]
separate1000s_on_double n = let r = round n in separate1000s r

fillToWith :: Integer -> Char -> String -> String
fillToWith n char s = if (length s) < n then fillToWith n char (char:s) else s

fillTo3 :: String -> String
--fillTo3 s = if length s < 3 then fillTo3 ('0':s) else s
fillTo3 = fillToWith 3 '0'

fill :: Integer -> String -> String
fill n s = fillToWith n ' ' s

fillNotFirst xs = (xs !! 0) : (map fillTo3 (tail xs))

double2dollar :: Double -> String
double2dollar n =
  let ns = separate1000s_on_double n 
  in "$" ++ ns $> map show $> fillNotFirst $> intersperse "," $> join

double2k :: Double -> String
double2k n = n $> round $> \n -> "$" ++ show n ++ "k"
