-- *************************************************************
-- *
-- * Trivial eXpression Language (TXL) Parser using Happy
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

-- Happy grammar with semantic actions for building an AST.

%name parser
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

%%

txl_program :: { Exp }
txl_program : exp		{ $1 }

exp :: { Exp }
exp : add_exp			{ $1 }

add_exp :: { Exp }
add_exp : mul_exp		{ $1 }
        | add_exp '+' mul_exp	{ BinOpApp Plus $1 $3 }
        | add_exp '-' mul_exp	{ BinOpApp Minus $1 $3 }

mul_exp :: { Exp }
mul_exp : prim_exp              { $1 }
        | mul_exp '*' prim_exp	{ BinOpApp Times $1 $3 }
        | mul_exp '/' prim_exp	{ BinOpApp Divide $1 $3 }

prim_exp :: { Exp }
prim_exp : int			{ LitInt $1 }
         | ident	        { Var $1 }
         | '(' exp ')'          { $2 }
         | let ident '=' exp in exp	{ Let $2 $4 $6 } 

{
-- Haskell code for defining token type, AST, scanner, top-level
-- functions. More or less verbatim from the original TXL example
-- (lecture 2).


type Id = String

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
-- Abstract Syntax Tree
----------------------------------------------------------------

data BinOp = Plus | Minus | Times | Divide deriving Show

data Exp = LitInt   Int
         | Var      Id
         | BinOpApp BinOp Exp Exp
         | Let      Id Exp Exp
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
-- Pretty printing of expressions
----------------------------------------------------------------

ppExp :: Exp -> String
ppExp e = ppExpAux 0 e ""

ppExpAux :: Int -> Exp -> ShowS
ppExpAux n (LitInt x) = indent n . showString (show x) . nl
ppExpAux n (Var i)    = indent n . showString i . nl
ppExpAux n (BinOpApp op e1 e2) =
    indent n . showString "BinOpApp" . nl
    . ppExpAux (n+1) e1
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e2
ppExpAux n (Let i e1 e2) =
    indent n . showString "Let" . nl
    . indent (n+1) . showString i . nl
    . ppExpAux (n+1) e1
    . ppExpAux (n+1) e2


----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

indent n = showString (take (2 * n) (repeat ' '))

nl  = showChar '\n'

spc = showChar ' '

happyError :: [Token] -> a
happyError _ = error "Parse error"

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

-- Usage:
--     happytxl file.txl	Parse "file.txl" and write result
--				to standard output.
--     happytxl			Read input from standard input and write
--				result to standard output.
--				(Could be confusing!)

main = do
    args  <- getArgs
    input <- if null args then getContents else readFile (head args)
    putStr ((ppExp . parser . scanner) input)

}
