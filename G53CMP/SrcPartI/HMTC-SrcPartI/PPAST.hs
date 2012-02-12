{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		PPAST						     *
*	Purpose:	Simple pretty printer for AST			     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Simple pretty printer for AST.

module PPAST (
    ppAST	-- AST -> String
) where

-- HMTC module imports
import Name (Name)
import SrcPos (SrcPos)
import PPUtilities
import AST


------------------------------------------------------------------------------
-- Pretty printing of AST
------------------------------------------------------------------------------

-- | Converts AST to a nicely laid-out textual representation for
-- display purposes.

ppAST :: AST -> String
ppAST ast = ppCommand 0 (astCmd ast) ""


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
ppExpression n (ExpLitInt {eliVal = v}) = 
    indent n . showString "ExpLitInt". spc . shows v . nl
ppExpression n (ExpVar {evVar = v}) =
    indent n . showString "ExpVar" . spc . ppName v . nl
ppExpression n (ExpApp {eaFun = f, eaArgs = es, expSrcPos = sp}) =
    indent n . showString "ExpApp" . spc . ppSrcPos sp . nl
    . ppExpression (n+1) f
    . ppSeq (n+1) ppExpression es


------------------------------------------------------------------------------
-- Pretty printing of declarations
------------------------------------------------------------------------------

ppDeclaration :: Int -> Declaration -> ShowS
ppDeclaration n (DeclConst {dcConst = c, dcType = t, dcVal = e,
                            declSrcPos = sp}) = 
    indent n . showString "DeclConst" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName c . nl
    . ppTypeDenoter (n+1) t
    . ppExpression (n+1) e
ppDeclaration n (DeclVar {dvVar = v, dvType = t, dvMbVal = me,
                          declSrcPos = sp}) = 
    indent n . showString "DeclVar" . spc . ppSrcPos sp . nl
    . indent (n+1) . ppName v . nl
    . ppTypeDenoter (n+1) t
    . maybe id (ppExpression (n+1)) me


------------------------------------------------------------------------------
-- Pretty printing of type denoters
------------------------------------------------------------------------------

ppTypeDenoter :: Int -> TypeDenoter -> ShowS
ppTypeDenoter n (TDBaseType {tdbtName = tn}) = 
    indent n . showString "TDBaseType" . spc . ppName tn . nl
