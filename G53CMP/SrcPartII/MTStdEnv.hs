{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		MTStdEnv					     *
*	Purpose:	MiniTriangle Initial Environment		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | MiniTriangle initial environment

module MTStdEnv (
    Env,	-- Re-exported
    mtStdEnv	-- :: Env
) where


-- HMTC module imports
import Name
import TAMCode (MTInt)
import Type
import Symbol (ExtSymVal (..))
import Env


-- | The MiniTriangle initial environment.
--
-- [Types:] Boolean, Integer
--
-- [Constants:]
--
--   * false, true : Boolean
--
--   * maxInt : Integer
--
-- [Functions (binary and unary operators):]
--
--   * (+), (-), (*), (\/), (\^) : (Integer, Integer) -> Integer
--
--   * (neg) : Integer -> Integer
--
--   * (\<), (\<=), (==), (!=), (>=), (>) : (Integer, Integer) -> Boolean
--
--   * (&&), (||) : (Boolean, Boolean) -> Boolean
--
--   * (!) : Boolean -> Boolean
--
-- [Procedures:]
--
--   * getint : (Sink Integer) -> Unit
--
--   * putint : Integer -> Unit

mtStdEnv :: Env
mtStdEnv =
    mkTopLvlEnv
        [("Boolean", Boolean),
         ("Integer", Integer)]
        [("false",   Boolean, ESVBool False),
         ("true",    Boolean, ESVBool True),
	 ("minint",  Integer, ESVInt (minBound :: MTInt)),
	 ("maxint",  Integer, ESVInt (maxBound :: MTInt)),
         ("+",       Arrow [Integer, Integer] Integer, ESVLbl "add"),
         ("-",       Arrow [Integer, Integer] Integer, ESVLbl "sub"),
         ("*",       Arrow [Integer, Integer] Integer, ESVLbl "mul"),
         ("/",       Arrow [Integer, Integer] Integer, ESVLbl "div"),
         ("^",       Arrow [Integer, Integer] Integer, ESVLbl "pow"),
         ("neg",     Arrow [Integer] Integer,          ESVLbl "neg"),
         ("<",       Arrow [Integer, Integer] Boolean, ESVLbl "lt"),
         ("<=",      Arrow [Integer, Integer] Boolean, ESVLbl "le"),
         ("==",      Arrow [Integer, Integer] Boolean, ESVLbl "eq"),
         ("!=",      Arrow [Integer, Integer] Boolean, ESVLbl "ne"),
         (">=",      Arrow [Integer, Integer] Boolean, ESVLbl "ge"),
         (">",       Arrow [Integer, Integer] Boolean, ESVLbl "gt"),
         ("&&",      Arrow [Boolean, Boolean] Boolean, ESVLbl "and"),
         ("||",      Arrow [Boolean, Boolean] Boolean, ESVLbl "or"),
         ("!",       Arrow [Boolean] Boolean,          ESVLbl "not"),
         ("getint",  Arrow [Sink Integer] Unit,        ESVLbl "getint"), 
         ("putint",  Arrow [Integer] Unit,             ESVLbl "putint")]
 
                        
