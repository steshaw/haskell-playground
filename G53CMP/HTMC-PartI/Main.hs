{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Main						     *
*	Purpose:	Main MiniTriangle compiler driver for Part I	     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Main MiniTriangle compiler driver.

--module Main (main) where

-- Standard library imports
import Maybe (isJust, fromJust)
import Monad (when)
import System (getArgs, exitWith, ExitCode(..))

-- HMTC module imports
import SrcPos (SrcPos(..))
import Diagnostics
import AST (AST)
import PPAST
-- import MTIR (MTIR)		-- For Part II
-- import PPMTIR		-- For Part II
import Parser
-- import TypeChecker		-- For Part II


data Options =
    Options {
	optHelp       :: Bool,
	optSAParsing  :: Bool,
        optSAChecking :: Bool,
        optPAParsing  :: Bool,
        optPAChecking :: Bool,
	optVersion    :: Bool
    }
    deriving Show


defaultOptions :: Options
defaultOptions =
    Options {
        optHelp       = False,
	optSAParsing  = False,
        optSAChecking = False,
        optPAParsing  = True,
        optPAChecking = False,
	optVersion    = False
    }

version :: String
version = "Haskell Mini Triangle Compiler (HMTC) version 1.00 (Part I)"


------------------------------------------------------------------------------
-- Main
------------------------------------------------------------------------------

-- | Usage (command line):
--
--	[hmtc \[options\] file.mt]	Compile \"file.mt\"
--
--	[hmtc \[options\]]		Read program to compile from standard
--                     		        input. (Could be confusing!)
--
-- Options:
--
--	[--help]		Print help message and stop.
--
--	[--stop-after-parsing]	Stop after parsing.
--				Implies \"--print-after-parsing\".
--
--	[--stop-after-checking]	Stop after type checking.
--				Implies	\"--print-after-checking\".
--
--	[--print-after-parsing]	Print intermediate representation after
--				parsing.
--
--	[--print-after-checking] Print intermediate representation after
--				type checking.
--
--	[--version]		Print HMTC version and stop.

main :: IO ()
main = do
    (opts, mf) <- parseCmdLine
    if optHelp opts then do
        printHelp
        exitWith $ ExitFailure 2
     else if optVersion opts then do
        putStrLn version
        exitWith $ ExitFailure 2
     else if optPAChecking opts then do
        putStrLn "--stop-after-parsing only meaningful in PartII (PartI _always_ stops after parsing)"
        exitWith $ ExitFailure 2
     else if optPAChecking opts then do
        putStrLn "--print-after-checking only valid in PartII"
        exitWith $ ExitFailure 2
     else if optSAChecking opts then do
        putStrLn "--stop-after-checking only valid in PartII"
        exitWith $ ExitFailure 2
     else do
        prog <- case mf of
                    Nothing -> getContents
                    Just f  -> readFile f
        let (mc, msgs) = runD (compile opts prog)
        mapM_ (putStrLn . ppDMsg) msgs
        case mc of
          Just mc -> exitWith $ ExitSuccess
          Nothing -> exitWith $ ExitFailure 1


------------------------------------------------------------------------------
-- Parse the command line
------------------------------------------------------------------------------

parseCmdLine :: IO (Options, Maybe String)
parseCmdLine = do
    args <- getArgs
    let (mof, msgs) = runD (processOptions defaultOptions args)
    mapM_ (putStrLn . ppDMsg) msgs
    case mof of
        Just (opts, as) -> return (opts,
                                   if null as then
                                       Nothing
                                   else
                                       (Just (head as)))
        Nothing -> ioError (userError "Aborted.")


processOptions :: Options -> [String] -> D (Options, [String])
processOptions opts as = do
    oas <- posAux opts as
    failIfErrorsD
    return oas
    where
        posAux :: Options -> [String] -> D (Options, [String])
        posAux opts [] = return (opts, [])
        posAux opts aas@(a:as)
            | take 2 a /= "--" = return (opts, aas)
            | otherwise        = do
                opts' <- poAux opts (drop 2 a)
                posAux opts' as

        poAux :: Options -> String -> D Options
        poAux opts o
            | o == "help" =
		return (opts {optHelp = True})
            | o == "stop-after-parsing" =
		return (opts {optSAParsing = True, optPAParsing = True})
            | o == "stop-after-checking" =
		return (opts {optSAChecking = True, optPAChecking = True})
            | o == "print-after-parsing" =
		return (opts {optPAParsing = True})
            | o == "print-after-checking" =
		return (opts {optPAChecking = True})
            | o == "version" =
		return (opts {optVersion = True})
            | otherwise = do
                emitErrD NoSrcPos ("Unknown option \"--" ++ o ++ "\"")
                return opts


------------------------------------------------------------------------------
-- Compiler
------------------------------------------------------------------------------

-- This version of the compiler driver if for Part I of the coursework.
-- It reports any errors from scanning and parsing and then stops, optionally
-- printing the AST.

compile :: Options -> String -> D ()
compile opts src = do
    -- Parsing
    ast <- parse src
    when (optPAParsing opts) (emitInfoD NoSrcPos (ppAST ast))
    failIfErrorsD
    when (optSAParsing opts) stopD

    -- Type checking (For Part II)
    -- mtir <- typeCheck ast
    -- when (optPAChecking opts) (emitInfoD NoSrcPos (ppMTIR mtir))
    -- failIfErrorsD
    -- when (optSAChecking opts) stopD

    -- Code generation here (For Part II)

    return ()


------------------------------------------------------------------------------
-- Print Help Text
------------------------------------------------------------------------------

helpText = "\
\Usage:\n\
\    hmtc [options] file.mt      Compile \"file.mt\"\n\
\    hmtc [options]              Read input from standard input.\n\
\                                (Could be confusing!)\n\
\Options:\n\
\    --help                      Print help message and stop.\n\
\    --stop-after-parsing        Stop after parsing.\n\
\                                Implies \"--print-after-parsing\".\n\
\    --stop-after-checking       Stop after type checking.\n\
\                                Implies \"--print-after-checking\".\n\
\    --print-after-parsing       Print intermediate representation after\n\
\                                parsing.\n\
\    --print-after-checking      Print intermediate representation after\n\
\                                type checking.\n\
\    --version                   Print HMTC version and stop.\n\
\"

printHelp :: IO ()
printHelp = putStr helpText
