-- *************************************************************
-- *
-- * Trivial eXpression Language (TXL) Interpreter using Happy
-- *
-- * Example for G53CMP, lecture 6, October 2011
-- *
-- *************************************************************

{
module Main where

import Char
import System
}

----------------------------------------------------------------
-- Parser
----------------------------------------------------------------

-- Happy grammar with semantic actions for INTERPRETING TXL programs.
-- NO AST is built.

%name interpreter
%tokentype { Token }

%token
    int		{ T_Int $$ }
    ident       { T_Id $$ }
    '+'		{ T_Plus }
    '-'		{ T_Minus }
    '*'		{ T_Times }
    '/'		{ T_Divide }
    '('		{ T_LeftPar }
    ')'		{ T_RightPar }
    '='		{ T_Equal }
    let		{ T_Let }
    in		{ T_In }

%right in
%left '+' '-'
%left '*' '/'

%%

txl_program :: { Int }
txl_program : exp		{ $1 (\_ -> error "undefined variable")}

exp :: { Env -> Int }
exp : exp '+' exp		{ \env -> ($1 env) + ($3 env) }
    | exp '-' exp		{ \env -> ($1 env) - ($3 env) }
    | exp '*' exp		{ \env -> ($1 env) * ($3 env) }
    | exp '/' exp		{ \env -> ($1 env) `div` ($3 env) }
    | int			{ \_ -> $1 }
    | ident	        	{ \env -> env $1 }
    | '(' exp ')'       	{ $2 }
    | let ident '=' exp in exp	{ \env -> let v = $4 env
                                          in $6 (\i -> if i == $2
                                                       then v
                                                       else env i)} 

{
-- Haskell code for defining token type, AST, scanner, top-level
-- functions. More or less verbatim from the original TXL example
-- (lecture 2).


type Id = String
type Env = Id -> Int

----------------------------------------------------------------
-- Token type
----------------------------------------------------------------

data Token = T_Int Int
           | T_Id Id
           | T_Plus
           | T_Minus
           | T_Times
           | T_Divide
           | T_LeftPar
           | T_RightPar
           | T_Equal
           | T_Let
           | T_In
           deriving Show 


----------------------------------------------------------------
-- Scanner
----------------------------------------------------------------

scanner :: [Char] -> [Token]
-- End of input
scanner []          = []
-- Drop white space and comments
scanner (' '  : cs) = scanner cs
scanner ('\n' : cs) = scanner cs
scanner ('!'  : cs) = scanner (dropWhile (/='\n') cs)
-- Scan single character tokens
scanner ('+' : cs) = T_Plus     : scanner cs
scanner ('-' : cs) = T_Minus    : scanner cs
scanner ('*' : cs) = T_Times    : scanner cs
scanner ('/' : cs) = T_Divide   : scanner cs
scanner ('(' : cs) = T_LeftPar  : scanner cs
scanner (')' : cs) = T_RightPar : scanner cs
scanner ('=' : cs) = T_Equal    : scanner cs
-- Scan literal integers, identifiers, and keywords
scanner (c : cs) | isDigit c =
                       T_Int (read (c : takeWhile isDigit cs))
                       : scanner (dropWhile isDigit cs)
                 | isAlpha c =
                       mkIdOrKwd (c : takeWhile isAlphaNum cs)
                       : scanner (dropWhile isAlphaNum cs)
                 | otherwise = error "Illegal character!"
    where
        mkIdOrKwd "let" = T_Let
        mkIdOrKwd "in"  = T_In
        mkIdOrKwd cs    = T_Id cs


----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

happyError :: [Token] -> a
happyError _ = error "Parse error"

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

-- Usage:
--     runtxl file.txl	Interpret "file.txl" and write result
--			to standard output.
--     runtxl		Interpret standard input and write
--			result to standard output.

main = do
    args  <- getArgs
    input <- if null args then getContents else readFile (head args)
    print ((interpreter . scanner) input)

}
