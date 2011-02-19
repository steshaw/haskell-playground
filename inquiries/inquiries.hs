{-
  The number of inquiries in 2008 is 806.
  The inquiries increase by 77.2% in 2009 and each year after the percent increase triples.
  This program outputs a table of the number of inquries expected under this senario from 2008 to 2014.
-}
module Main where

import Control.Monad (mapM_)
import FormatDecimal (formatDecimal)
import Text.Printf (printf)

($>) = flip ($)

initial = 806 -- inquiries for year 2008
increase = 0.772 -- 77.2%
multiple = 3

next previous index =
  let n = previous * (1 + increase * multiple ** index)
  in  n : next n (index + 1)

inquiries = initial : next initial 0

inquiriesByYear = zip [2008..] inquiries

format (year, inquiries) = (show year) ++ ": " ++ printf "%15s" right
  where right = (formatDecimal 0 (inquiries :: Float))

numYearsToPrint = 7

main = take numYearsToPrint inquiriesByYear $> map format $> mapM_ putStrLn
