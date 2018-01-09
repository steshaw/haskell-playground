#!/usr/bin/env stack
{-
  stack --resolver lts-10.3 script
    --package scientific
-}

import Data.Scientific

main = do
  c <- getContents
  let l = lines c
  let a = map read l :: [Scientific]
  putStrLn $ formatScientific Fixed Nothing $ sum a
