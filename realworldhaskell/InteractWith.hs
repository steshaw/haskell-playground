module InteractWith where

import System.Environment(getArgs, getProgName)

interactWith f inputPath outputPath = do
  input <- readFile inputPath
  writeFile outputPath (f input)

interactWithMain f = do
  args <- getArgs
  progName <- getProgName
  case args of
    [input, output] -> interactWith f input output
    otherwise -> putStrLn $ "usage: " ++ progName ++ " <input-file> <output-file>"
