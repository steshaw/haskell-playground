--
-- Hacked by Steven Shaw to accept filename as arg.
--

-- Main module for Imp parser and interpreter.
-- Developed for use in COMP3610
-- Clem Baker-Finch

module Main (main) where

import IO
import Parser
import Interpreter
import System (getArgs)

main :: IO()
main = do
  [filename] <- getArgs
  input <- readFile filename
  let result = eC (parseProg input) arid
  putStr (show result)
