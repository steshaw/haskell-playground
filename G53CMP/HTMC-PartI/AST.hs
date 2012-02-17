{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		AST						     *
*	Purpose:	MiniTriangle Abstract Syntax Tree		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | MiniTriangle Abstract Syntax Tree. Representation of MiniTriangle programs
-- after parsing but prior to type-checking.

module AST (
    AST (..),		-- Not abstract. Instances: HasSrcPos.
    Command (..),	-- Not abstract. Instances: HasSrcPos.
    Expression (..),	-- Not abstract. Instances: HasSrcPos.
    Declaration (..),	-- Not abstract. Instances: HasSrcPos.
    TypeDenoter (..)	-- Not abstract. Instances: HasSrcPos.
) where

-- HMTC module imports
import Name
import SrcPos

-- Note on Naming Conventions for Constructors and Field Labels
--
-- In Haskell, two (or more) datatypes that are in scope simultaneoulsy
-- must not have any constructors or field labels in common. However,
-- different constructors of the same type may have common field names,
-- provided the fields all have the same type. This is very different
-- from records in languages like Pascal or C, and from objects in OO
-- languages like Java, where sharing names across different records or
-- objects are both possible and common.
--
-- To avoid name clashes, while still making it possible to use similar
-- names for similar things in different type declarations, some simple
-- naming conventins have been adopted:
--
--   * Constructors get prefix which is an abbreviation of the name of
--     the data type. E.g. for 'Command', the prefix is 'Cmd', and a
--     typical constructor name is 'CmdAssign', and for 'TypeDenoter',
--     te prefix is 'TD'.
--
--   * Field names that are common to one or more constructors, get the
--     same prefix as the constructor, but in lower-case.
--
--   * Field names that are specific to a contructor get a lower-case
--     prefix that is an abbreviation of the constructor. E.g. the
--     prefix for 'CmdAssign' is 'ca', and one of its fields is 'caVar'.

-- | Abstract syntax for the syntactic category Program
data AST = AST { astCmd :: Command }


instance HasSrcPos AST where
    srcPos = cmdSrcPos . astCmd


-- | Abstract syntax for the syntactic category Command

-- For generality, the variable being assigned to, the procedure being
-- called, and the function being applied (currently only operators) are
-- represented by expressions as opposed to just an identifier (for
-- variables, procedures, and functions) or an operator. Consider
-- assignment to an array element, for example, where the RHS (e.g. x[i])
-- really is an expression that gets evaluated to a memory reference
-- (sink). Also, this arrangement facilitates error reporting, as a
-- variable expression has an associated source position, whereas names,
-- currently represented by strings, have not.

data Command
    -- | Assignment
    = CmdAssign {
          caVar     :: Expression,	-- ^ Assigned variable
          caVal     :: Expression,	-- ^ Right-hand-side expression
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


-- | Abstract syntax for the syntactic category Expression
data Expression
    -- | Literal integer
    = ExpLitInt {
	  eliVal    :: Integer,		-- ^ Integer value
	  expSrcPos :: SrcPos
      }
    -- | Variable reference
    | ExpVar {
	  evVar     :: Name,		-- ^ Name of referenced variable
	  expSrcPos :: SrcPos
      }
    -- | Function or n-ary operator application
    | ExpApp {
	  eaFun     :: Expression,	-- ^ Applied function or operator
          eaArgs    :: [Expression],	-- ^ Arguments
	  expSrcPos :: SrcPos
      }


instance HasSrcPos Expression where
    srcPos = expSrcPos


-- | Abstract syntax for the syntactic category Declaration
data Declaration
    -- | Constant declaration
    = DeclConst {
	  dcConst    :: Name,		-- ^ Name of defined constant
	  dcType     :: TypeDenoter,	-- ^ Type of defined constant
	  dcVal	     :: Expression,	-- ^ Value of defined constant
          declSrcPos :: SrcPos
      }
    -- | Variable declaration
    | DeclVar {
	  dvVar	     :: Name,		-- ^ Name of declared variable
	  dvType     :: TypeDenoter,	-- ^ Type of declared variable
	  dvMbVal    :: Maybe Expression, -- ^ Initial value of declared
                                          -- varible, if any
          declSrcPos :: SrcPos
      }


instance HasSrcPos Declaration where
    srcPos = declSrcPos


-- | Abstract syntax for the syntactic category TypeDenoter

-- Right now, the only types are simple base types like Integer and Bool.
-- If MiniTriangle were extended to allow users to express e.g. function
-- types, then this data declaration would have to be extended.
data TypeDenoter
    -- | Base Type
    = TDBaseType {
          tdbtName :: Name,	-- ^ Name of the base type
	  tdSrcPos :: SrcPos
      }


instance HasSrcPos TypeDenoter where
    srcPos = tdSrcPos

