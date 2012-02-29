-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		Diagnostics					     *
-- *	Purpose:	Support for diagnostics and error reporting	     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module Diagnostics (
    ErrorMsg,
    internalError -- :: ErrorMsg -> a
) where

type ErrorMsg = String

internalError :: ErrorMsg -> a
internalError msg = error ("[Internal Error ]"
                           ++ msg
                           ++ " (should not happen)")
