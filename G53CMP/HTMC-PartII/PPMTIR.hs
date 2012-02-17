{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		PPMTIR						     *
*	Purpose:	Simple pretty printer for MTIR			     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Simple pretty printer for MTIR.

module PPMTIR (
    ppMTIR	-- MTIR -> String
) where

-- HMTC module imports
import Name (Name)
import SrcPos
import Type (Type)
import Symbol (TermSym)
import PPUtilities
import MTIR


------------------------------------------------------------------------------
-- Pretty printing of MTIR
------------------------------------------------------------------------------

-- | Converts MTIR to a nicely laid-out textual representation for
-- display purposes.

ppMTIR :: MTIR -> String
ppMTIR mtir = ppCommand 0 (mtirCmd mtir) ""


------------------------------------------------------------------------------
-- Pretty printing of commands
------------------------------------------------------------------------------

ppCommand :: Int -> Command -> ShowS
ppCommand n (CmdAssign {caVar = v, caVal = e, cmdSrcPos = sp}) =
    indent n . showString "CmdAssign" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) v
    . ppExpression (n+1) e
ppCommand n (CmdCall {ccProc = p, ccArgs = es, cmdSrcPos = sp}) =
    indent n . showString "CmdCall" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) p
    . ppSeq (n+1) ppExpression es
ppCommand n (CmdSeq {csCmds = cs, cmdSrcPos = sp}) =
    indent n . showString "CmdSeq" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppCommand cs
ppCommand n (CmdIf {ciCond = e, ciThen = c1, ciElse = c2, cmdSrcPos = sp}) =
    indent n . showString "CmdIf" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e
    . ppCommand (n+1) c1
    . ppCommand (n+1) c2
ppCommand n (CmdWhile {cwCond = e, cwBody = c, cmdSrcPos = sp}) =
    indent n . showString "CmdWhile" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e
    . ppCommand (n+1) c
ppCommand n (CmdLet {clDecls = ds, clBody = c, cmdSrcPos = sp}) =
    indent n . showString "CmdLet" . spc . ppSrcPos sp . nl
    . ppSeq (n+1) ppDeclaration ds
    . ppCommand (n+1) c


------------------------------------------------------------------------------
-- Pretty printing of expressions
------------------------------------------------------------------------------

ppExpression :: Int -> Expression -> ShowS
ppExpression n (ExpLitBool {elbVal = v, expType = t}) = 
    indent n . showString "ExpLitBool". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpLitInt {eliVal = v, expType = t}) = 
    indent n . showString "ExpLitInt". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpLitChar {elcVal = v, expType = t}) = 
    indent n . showString "ExpLitChar". spc . shows v
    . showString " : " . shows t . nl
ppExpression n (ExpExtRef {eerVal = l, expType = t}) = 
    indent n . showString "ExpExtRef". spc . shows l
    . showString " : " . shows t . nl
ppExpression n (ExpVar {evVar = v}) =
    indent n . showString "ExpVar" . spc . shows v . nl
ppExpression n (ExpDeref {edArg = e, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpDeref" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) e
    . indent n . showString ": " . shows t . nl
ppExpression n (ExpApp {eaFun = f, eaArgs = es, expType = t, expSrcPos = sp}) =
    indent n . showString "ExpApp" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) f
    . ppSeq (n+1) ppExpression es
    . indent n . showString ": " . shows t . nl


------------------------------------------------------------------------------
-- Pretty printing of declarations
------------------------------------------------------------------------------

ppDeclaration :: Int -> Declaration -> ShowS
ppDeclaration n (DeclConst {dcConst = c, dcVal = e}) = 
    indent n . showString "DeclConst" . spc . ppSrcPos (srcPos c) . nl
    . indent (n+1) . shows c . nl
    . ppExpression (n+1) e
ppDeclaration n (DeclVar {dvVar = v, dvMbVal = me}) = 
    indent n . showString "DeclVar" . spc . ppSrcPos (srcPos v) . nl
    . indent (n+1) . shows v . nl
    . maybe id (ppExpression (n+1)) me
