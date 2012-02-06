--
-- Hacked by Steven Shaw to accept filename as arg.
--

-- Main module for Imp parser and interpreter.
-- Developed for use in COMP3610
-- Clem Baker-Finch

module Main (main) where

import Scanner
import Parser
import Interpreter
import System (getArgs)
import Control.Monad (forM_)
import Control.Arrow ((&&&))
import Text.Printf

printTokens :: [Token] -> IO ()
printTokens tokens = do
  putStrLn "scanner: "
  print tokens
  map (id &&& unscan) tokens `forM_` \(token, unscanned) ->
    printf "  %-30s %-30s\n" unscanned (show token)

main :: IO ()
main = do
  [filename] <- getArgs
  input <- readFile filename
  let tokens = scan input
  printTokens tokens
  let result = eval (parse tokens) initEnv
  putStrLn "eval ==>"
  putStr (show result)
