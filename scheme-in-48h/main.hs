module Main where

import System.Environment

main :: IO ()
main = do
  args <- getArgs
  case args of [a,b] -> putStrLn $ "Sum is " ++ (show ((read a) + (read b)))
