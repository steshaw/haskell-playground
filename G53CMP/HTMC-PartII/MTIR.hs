{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		MTIR						     *
*	Purpose:	MiniTriangle Internal Representation 		     *
*			(typechecked AST with semantical annotations)        *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}


-- | MiniTriangle Internal Representation. The definitions mirror the
-- AST, but the program should be type correct at this stage and semantical
-- annotations have been added. In particular, variables are represented
-- by symbols (with name, type info, and source position) as opposed to just
-- their names.

module MTIR (
    -- MTIR types
    MTIR (..),		-- Not abstract. Instances: HasSrcPos.
    Command (..),	-- Not abstract. Instances: HasSrcPos.
    Expression (..),	-- Not abstract. Instances: HasSrcPos.
    Declaration (..),	-- Not abstract. Instances: HasSrcPos.
    MTInt		-- Representation type for MiniTriangle integers.
) where

-- HMTC module imports
import SrcPos
import Name
import Type
import Symbol (TermSym)
import TAMCode (MTInt)


-- | Internal representation of MiniTriangle Programs
data MTIR = MTIR { mtirCmd :: Command }


instance HasSrcPos MTIR where
    srcPos = cmdSrcPos . mtirCmd


-- | Internal representation of Commands
data Command
    -- | Assignment
    = CmdAssign {
          caVar     :: Expression,	-- ^ Assigned variable
          caVal     :: Expression,	-- ^ Right-hand side expression
          cmdSrcPos :: SrcPos
      }
    -- | Procedure call
    | CmdCall {
          ccProc    :: Expression,	-- ^ Called procedure
          ccArgs    :: [Expression],	-- ^ Arguments
          cmdSrcPos :: SrcPos
      }
    -- | Command sequence (block)
    | CmdSeq {
          csCmds    :: [Command],	-- ^ Commands
          cmdSrcPos :: SrcPos
      }
    -- | Conditional command
    | CmdIf {
	  ciCond    :: Expression,	-- ^ Condition
	  ciThen    :: Command,		-- ^ Then-branch
	  ciElse    :: Command,		-- ^ Else-branch
          cmdSrcPos :: SrcPos
      }
    -- | While-loop
    | CmdWhile {
          cwCond    :: Expression,	-- ^ Loop-condition
          cwBody    :: Command,		-- ^ Loop-body
	  cmdSrcPos :: SrcPos
      }
    -- | Let-command
    | CmdLet {
          clDecls   :: [Declaration],	-- ^ Declarations
          clBody    :: Command,		-- ^ Let-body
	  cmdSrcPos :: SrcPos
      }


instance HasSrcPos Command where
    srcPos = cmdSrcPos


data Expression
    -- | Literal Boolean
    = ExpLitBool {
	  elbVal    :: Bool,		-- ^ Literal Boolean.
          expType   :: Type,		-- ^ Type
	  expSrcPos :: SrcPos
      }
    -- | Literal integer
    | ExpLitInt {
	  eliVal    :: MTInt,		-- ^ Literal integer.
          expType   :: Type,		-- ^ Type
	  expSrcPos :: SrcPos
      }
    -- | Literal character
    | ExpLitChar {
	  elcVal    :: Char,		-- ^ Literal character.
          expType   :: Type,		-- ^ Type
	  expSrcPos :: SrcPos
      }
    -- | External reference (procedure/function)
    | ExpExtRef {
	  eerVal    :: Name,		-- ^ Name of external entity.
          expType   :: Type,		-- ^ Type
	  expSrcPos :: SrcPos
      }
    -- | Variable reference
    | ExpVar {
	  evVar     :: TermSym,		-- ^ Referenced variable (symbol!)
          expType   :: Type,		-- ^ Type
	  expSrcPos :: SrcPos
      }
    -- | Dereferencing of reference variable
    | ExpDeref {
          edArg     :: Expression,	-- ^ Argument
          expType   :: Type,		-- ^ Type (after dereferencing)
	  expSrcPos :: SrcPos
      }
    -- | Function or n-ary operator application
    | ExpApp {
	  eaFun     :: Expression,	-- ^ Applied function or operator
          eaArgs    :: [Expression],	-- ^ Arguments
          expType   :: Type,		-- ^ Type (of application)
	  expSrcPos :: SrcPos
      }


instance HasSrcPos Expression where
    srcPos = expSrcPos


data Declaration
    -- | Constant declaration
    = DeclConst {
	  dcConst    :: TermSym,	-- ^ Defined constant (symbol!)
                                        -- (The symbol carries the type.)
	  dcVal	     :: Expression	-- ^ Value of defined constant
      }
    -- Variable declaration
    | DeclVar {
	  dvVar	     :: TermSym,	 -- ^ Declared variable (symbol!)
                                         -- (The symbol carries the type.)
	  dvMbVal    :: Maybe Expression -- ^ Initial value of declared
                                         -- variable, if any
      }


instance HasSrcPos Declaration where
    srcPos (DeclConst {dcConst = s}) = srcPos s
    srcPos (DeclVar   {dvVar = s})   = srcPos s
