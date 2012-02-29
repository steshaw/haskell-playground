-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		Main						     *
-- *	Purpose:	Main module: driver for the type checker	     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************
module Main where

import System (getArgs)

import Type
import AbstractSyntax
import Parser (parse)
import Environment
import GlobalEnvironment
import TypeChecker


-- Parses, prints and runs the type checker on test programs and reports
-- resulting type or encountered errors.

tcProg :: String -> IO ()
tcProg prog = do
    let exp = parse prog
    putStrLn "Program to be type checked:"
    putStrLn ""
    putStrLn (ppExp exp)
    putStrLn ""
    let (tp, msgs) = typeCheck exp
    if null msgs then
        do
            putStr "Type of the program: "
            putStrLn (ppType tp)
     else
        do
            putStrLn "There were errors:"
            mapM_ putStrLn msgs


-- Reads program from file and then parses, prints and runs the type checker
-- on it and reports resulting type or encountered errors.

tcFile :: String -> IO ()
tcFile fileName = do
    prog <- readFile (fileName)
    tcProg prog


-- Test environments

Left env1 = enterVar "x" 1 TpInt glblEnv

Left env2 = enterVar "y" 1 TpBool env1

Left env3 = enterVar "x" 2 TpBool env2


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

-- Usage:
--     tcltxl file.ltxl	Parse "file.ltxl", type check it, and write result
--                      to standard output.
--     tcltxl		Read input from standard input, type check it,
--                      and write result to standard output.
--			(Could be confusing!)

main = do
    args <- getArgs
    prog <- if null args
             then getContents
             else readFile (head args)
    tcProg prog
