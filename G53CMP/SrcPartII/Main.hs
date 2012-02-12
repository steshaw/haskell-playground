{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Main						     *
*	Purpose:	Main MiniTriangle compiler driver.		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Main MiniTriangle compiler driver.

module Main (main) where

-- Standard library imports
import Maybe (isJust, fromJust)
import Monad (when)
import System (getArgs)

-- HMTC module imports
import SrcPos (SrcPos(..))
import Diagnostics
import AST (AST)
import PPAST
import MTIR (MTIR)
import PPMTIR
import TAMCode (TAMInst)
import PPTAMCode (ppTAMCode)
import Parser
import TypeChecker
import CodeGenerator
import TAMInterpreter
import LibMT

data Options =
    Options {
	optHelp       :: Bool,
	optSAParsing  :: Bool,
        optPAParsing  :: Bool,
        optSAChecking :: Bool,
        optPAChecking :: Bool,
        optSACodeGen  :: Bool,
        optPACodeGen  :: Bool,
        optRun        :: Bool,
        optTrace      :: Bool,
	optVersion    :: Bool
    }
    deriving Show


defaultOptions :: Options
defaultOptions =
    Options {
        optHelp       = False,
	optSAParsing  = False,
        optPAParsing  = False,
        optSAChecking = False,
        optPAChecking = False,
        optSACodeGen  = False,
        optPACodeGen  = False,
        optRun        = False,
        optTrace      = False,
	optVersion    = False
    }

version :: String
version = "Haskell Mini Triangle Compiler (HMTC) version 1.00 (Complete)"


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
--	[--help]			Print help message and stop.
--
--	[--stop-after-parsing]		Stop after parsing.
--					Implies \"--print-after-parsing\".
--
--	[--print-after-parsing]		Print intermediate representation after
--					parsing.
--
--	[--stop-after-checking]		Stop after type checking.
--					Implies	\"--print-after-checking\".
--
--	[--print-after-checking]	Print intermediate representation after
--					type checking.
--
--	[--stop-after-codegen]		Stop after code generation.
--					Implies \"--print-after-codegen\".
--
--	[--print-after-codegen]		Print generated TAM code.
--
--	[--run]				Interpret generated TAM code.
--
--	[--run-traced]			Interpret generated TAM code and trace
--					the execution.
--
--	[--version]			Print HMTC version and stop.

main :: IO ()
main = do
    (opts, mf) <- parseCmdLine
    if optHelp opts then
        printHelp
     else if optVersion opts then
        putStrLn version
     else do
        prog <- case mf of
                    Nothing -> getContents
                    Just f  -> readFile f
        let (mbCode, msgs) = runD (compile opts prog)
        mapM_ (putStrLn . ppDMsg) msgs
        case mbCode of
            Nothing   -> putStrLn "No code generated."
            Just code ->
                if optRun opts then
                    let
                        code' = code ++ libMT	-- Linking! :-)
                    in
                        runTAM (optTrace opts) code'
                else
                    case mf of
                        Nothing -> putStr (ppTAMCode code)
                        Just f  -> do
                            let f' = takeWhile (/='.') f ++ ".tam"
                            writeFile f' (ppTAMCode code)
			    putStrLn ("Code written to file \""
                                      ++ f' ++ "\"")


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
            | o == "print-after-parsing" =
		return (opts {optPAParsing = True})
            | o == "stop-after-checking" =
		return (opts {optSAChecking = True, optPAChecking = True})
            | o == "print-after-checking" =
		return (opts {optPAChecking = True})
            | o == "stop-after-codegen" =
		return (opts {optSACodeGen = True, optPACodeGen = True})
            | o == "print-after-codegen" =
		return (opts {optPACodeGen = True})
            | o == "run" =
		return (opts {optRun = True, optTrace = False})
            | o == "run-traced" =
		return (opts {optRun = True, optTrace = True})
            | o == "version" =
		return (opts {optVersion = True})
            | otherwise = do
                emitErrD NoSrcPos ("Unknown option \"--" ++ o ++ "\"")
                return opts


------------------------------------------------------------------------------
-- Compiler
------------------------------------------------------------------------------

-- Should eventually return some form of "Code".

compile :: Options -> String -> D [TAMInst]
compile opts src = do
    -- Parsing
    ast <- parse src
    when (optPAParsing opts) (emitInfoD NoSrcPos (ppAST ast))
    failIfErrorsD
    when (optSAParsing opts) stopD

    -- Type checking
    mtir <- typeCheck ast
    when (optPAChecking opts) (emitInfoD NoSrcPos (ppMTIR mtir))
    failIfErrorsD
    when (optSAChecking opts) stopD

    -- Code generation
    code <- genCode mtir
    when (optPACodeGen opts) (emitInfoD NoSrcPos (ppTAMCode code))
    failIfErrorsD
    when (optSACodeGen opts) stopD

    return code

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
\    --print-after-parsing       Print intermediate representation after\n\
\                                parsing.\n\
\    --stop-after-checking       Stop after type checking.\n\
\                                Implies \"--print-after-checking\".\n\
\    --print-after-checking      Print intermediate representation after\n\
\                                type checking.\n\
\    --stop-after-codegen        Stop after code generation.\n\
\                                Implies \"--print-after-codegen\".\n\
\    --print-after-codegen       Print generated TAM code.\n\
\    --run                       Interpret generated TAM code.\n\
\    --run-traced                Interpret generated TAM code and trace\n\
\                                the execution.\n\
\    --version                   Print HMTC version and stop.\n\
\"

printHelp :: IO ()
printHelp = putStr helpText
