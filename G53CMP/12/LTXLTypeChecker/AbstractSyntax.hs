-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		AbstractSyntax					     *
-- *	Purpose:	Definition of abstract syntax for LTXL		     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module AbstractSyntax (
    Id,
    UnOp(..),
    BinOp(..),
    Exp(..),
    ppExp -- :: Exp -> String
) where

import Type

type Id = String

data UnOp = Not | Neg deriving (Eq, Show)

data BinOp = Or
           | And 
           | Less 
           | Equal 
           | Greater
           | Plus 
           | Minus 
           | Times 
           | Divide
           deriving (Eq, Show)

data Exp = LitInt   Int
         | Var      Id
         | UnOpApp  UnOp Exp
         | BinOpApp BinOp Exp Exp
         | If       Exp Exp Exp
         | Let      [(Id, Type, Exp)] Exp
         deriving Show


------------------------------------------------------------------------------
-- Pretty printing of expressions
------------------------------------------------------------------------------

ppExp :: Exp -> String
ppExp e = ppExpAux 0 e ""

ppExpAux :: Int -> Exp -> ShowS
ppExpAux n (LitInt x) = indent n . showString (show x) . nl
ppExpAux n (Var i)  =
    indent n
    . showString i 
    . nl
ppExpAux n (UnOpApp op e) =
    indent n . showString "UnOpApp" . nl
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e
ppExpAux n (BinOpApp op e1 e2) =
    indent n . showString "BinOpApp" . nl
    . ppExpAux (n+1) e1
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e2
ppExpAux n (If e1 e2 e3) =
    indent n . showString "If" . nl
    . ppExpAux (n+1) e1
    . ppExpAux (n+1) e2
    . ppExpAux (n+1) e3
ppExpAux n (Let ds e) =
    indent n . showString "Let" . nl
    . ppDefs   (n+1) ds
    . ppExpAux (n+1) e

ppDefs n [] = id
ppDefs n ((i, t, e) : ds) =
    indent n . showString i . nl
    . indent (n+1) . shows t . nl
    . ppExpAux (n+1) e
    . ppDefs n ds


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

indent n = showString (take (2 * n) (repeat ' '))

nl  = showChar '\n'

spc = showChar ' '
