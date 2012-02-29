-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		Parser						     *
-- *	Purpose:	Scanner and Happy Parser for LTXL		     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

{
    module Parser (parse) where

import Char

import AbstractSyntax
import Type

-- Top-level parsing function
parse :: String -> Exp
parse = parser . scanner

}

------------------------------------------------------------------------------
-- Parser
------------------------------------------------------------------------------

-- Happy grammar with semantic actions for building an AST.

%name parser
%tokentype { Token }

%token
    litint	{ T_LitInt $$ }
    ident       { T_Id $$ }
    '||'	{ T_Or }
    '&&'	{ T_And }
    '<'		{ T_Less }
    '=='	{ T_Equal }
    '>'         { T_Greater }
    '+'		{ T_Plus }
    '-'		{ T_Minus }
    '*'		{ T_Times }
    '/'		{ T_Divide }
    '\\'	{ T_Not }
    '('		{ T_LeftPar }
    ')'		{ T_RightPar }
    if		{ T_If }
    then        { T_Then }
    else        { T_Else }
    let		{ T_Let }
    in		{ T_In }
    int		{ T_IntType }
    bool	{ T_BoolType }
    '='		{ T_Is }
    ';'         { T_SemiColon }

%right in else
%left '||'
%left '&&'
%left '<' '==' '>'
%left '+' '-'
%left '*' '/'

%%

ltxl_program : exp		{ $1 }

exp : exp '||' exp              { BinOpApp Or $1 $3 }
    | exp '&&' exp		{ BinOpApp And $1 $3 }
    | exp '<' exp		{ BinOpApp Less $1 $3 }
    | exp '==' exp		{ BinOpApp Equal $1 $3 }
    | exp '>' exp		{ BinOpApp Greater $1 $3 }
    | exp '+' exp		{ BinOpApp Plus $1 $3 }
    | exp '-' exp		{ BinOpApp Minus $1 $3 }
    | exp '*' exp		{ BinOpApp Times $1 $3 }
    | exp '/' exp		{ BinOpApp Divide $1 $3 }
    | pexp                      { $1 }

pexp : litint			{ LitInt $1 }
     | ident	        	{ Var $1 }
     | '\\' pexp                { UnOpApp Not $2 }
     | '-' pexp                 { UnOpApp Neg $2 }
     | if exp then exp else exp { If $2 $4 $6 }
     | '(' exp ')'       	{ $2 }
     | let decls in exp	        { Let $2 $4 } 

decls : decl ';' decls          { $1 : $3 }
      |decl                     { [$1] }

decl : type ident '=' exp	{ ($2, $1, $4) }

type : int			{ TpInt }
     | bool			{ TpBool }

{

-------------------------------------------------------------
-- Token type
-------------------------------------------------------------

data Token = T_LitInt Int
           | T_Id Id
           | T_Or
	   | T_And
           | T_Less
           | T_Equal
           | T_Greater
           | T_Plus
           | T_Minus
           | T_Times
           | T_Divide
           | T_Not
           | T_LeftPar
           | T_RightPar
           | T_Let
           | T_In
           | T_IntType
           | T_BoolType
           | T_Is
           | T_SemiColon
           | T_If
           | T_Then
           | T_Else
           deriving Show 


-------------------------------------------------------------
-- Scanner
-------------------------------------------------------------

scanner :: [Char] -> [Token]
-- End of input
scanner []          = []
-- Drop white space and comments
scanner (' '  : cs) = scanner cs
scanner ('\n' : cs) = scanner cs
scanner ('!'  : cs) = scanner (dropWhile (/='\n') cs)
-- Scan graphic tokens
scanner ('|'  : '|' : cs) = T_Or        : scanner cs
scanner ('&'  : '&' : cs) = T_And       : scanner cs
scanner ('<'  : cs)       = T_Less      : scanner cs
scanner ('='  : '=' : cs) = T_Equal     : scanner cs
scanner ('>'  : cs)       = T_Greater   : scanner cs
scanner ('+'  : cs)       = T_Plus      : scanner cs
scanner ('-'  : cs)       = T_Minus     : scanner cs
scanner ('*'  : cs)       = T_Times     : scanner cs
scanner ('/'  : cs)       = T_Divide    : scanner cs
scanner ('\\' : cs)       = T_Not       : scanner cs
scanner ('('  : cs)       = T_LeftPar   : scanner cs
scanner (')'  : cs)       = T_RightPar  : scanner cs
scanner ('='  : cs)       = T_Is        : scanner cs
scanner (';'  : cs)       = T_SemiColon : scanner cs
-- Scan literal integers, identifiers, and keywords
scanner (c : cs) | isDigit c =
                       T_LitInt (read (c :
                                       takeWhile isDigit cs))
                       : scanner (dropWhile isDigit cs)
                 | isAlpha c =
                       mkIdOrKwd (c :
                                  takeWhile isAlphaNum cs)
                       : scanner (dropWhile isAlphaNum cs)
                 | otherwise = error "Illegal character!"
    where
        mkIdOrKwd "if"   = T_If
        mkIdOrKwd "then" = T_Then
        mkIdOrKwd "else" = T_Else
        mkIdOrKwd "let"  = T_Let
        mkIdOrKwd "in"   = T_In
        mkIdOrKwd "int"  = T_IntType
        mkIdOrKwd "bool" = T_BoolType
        mkIdOrKwd cs     = T_Id cs


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

happyError :: [Token] -> a
happyError _ = error "Parse error"

}
