-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		Type						     *
-- *	Purpose:	Representation of types				     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module Type (
    Type(..),
    ppType -- :: Type -> String
) where

data Type = TpUnknown		-- "Type" of ill-typed terms
	  | TpBool		-- Boolean type
          | TpInt               -- Integer type
          | TpProd Type Type    -- Product type ("pair", (T1,T2))
          | TpArr  Type Type	-- Function type (T1 -> T2)
          deriving (Eq, Show)


------------------------------------------------------------------------------
-- Pretty printing of types
------------------------------------------------------------------------------

ppType :: Type -> String
ppType TpUnknown      = "???"
ppType TpBool         = "bool"
ppType TpInt          = "int"
ppType (TpProd t1 t2) = "(" ++ ppType t1 ++ "," ++ ppType t2 ++ ")"
ppType (TpArr t1 t2)  = "(" ++ ppType t1 ++ "->" ++ ppType t2 ++ ")"
