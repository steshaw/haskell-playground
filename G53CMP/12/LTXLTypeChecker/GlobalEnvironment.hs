-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		GlobalEnvironment				     *
-- *	Purpose:	The global environment				     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module GlobalEnvironment where

import AbstractSyntax (Id, UnOp(..), BinOp(..))
import Type (Type(..))
import Environment (Env, initEnv)


glblEnv :: Env
glblEnv =
    initEnv []				-- No globally defined variables
            [ (Not, tpBtoB),		-- The unary operators
              (Neg, tpItoI) ]
            [ (Or,      tpBBtoB),	-- The binary operators
              (And,     tpBBtoB),
              (Less,    tpIItoB),
              (Equal,   tpIItoB),
              (Greater, tpIItoB),
              (Plus,    tpIItoI),
              (Minus,   tpIItoI),
              (Times,   tpIItoI),
              (Divide,  tpIItoI) ]
    where
        tpBtoB  = TpArr TpBool TpBool
        tpItoI  = TpArr TpInt TpInt
        tpBBtoB = TpArr (TpProd TpBool TpBool) TpBool
        tpIItoB = TpArr (TpProd TpInt TpInt) TpBool
        tpIItoI = TpArr (TpProd TpInt TpInt) TpInt
