-- Main module for Imp parser and interpreter.
-- Developed for use in COMP3610
-- Clem Baker-Finch

module Main (main) where

import IO
import Parser
import Interpreter

-- The usual general file opening function.

getAndOpenFile :: String -> IOMode -> IO Handle
getAndOpenFile prompt mode =
    do putStr prompt
       name <- getLine
       catch (openFile name mode)
	     (\_ -> do putStrLn ("Cannot open " ++ name ++ "\n")
                       getAndOpenFile prompt mode)

-- Main function: get the source Imp file, parse it, interpret it and
-- print the resulting state to stdout.

main :: IO()
main = do
       sourceFile <- getAndOpenFile "Path of Imp source file: " ReadMode
       imp <- hGetContents sourceFile
       let result = eC (parseProg imp) arid
       putStr (show result)

