{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		LibMT						     *
*	Purpose:	TAM code for LibMT: the MiniTriangle std library.    *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | LibMT: MiniTriangle Standard Library

module LibMT (
    libMT	-- :: [TAMInst]
) where

-- HMTC module imports
import Name
import TAMCode

libMT = [
-- add
    LABEL "add",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    ADD,
    RETURN 1 2,

-- sub
    LABEL "sub",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    SUB,
    RETURN 1 2,

-- mul
    LABEL "mul",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    MUL,
    RETURN 1 2,

-- div
    LABEL "div",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    DIV,
    RETURN 1 2,

-- pow:
    LABEL "pow",
    LOADL 1,
    LABEL "pow_loop",
    LOAD (LB (-1)),
    LOADL 0,
    GTR,
    JUMPIFZ "pow_out",
    LOAD (LB (-1)),
    LOADL 1,
    SUB,
    STORE (LB (-1)),
    LOAD (LB (-2)),
    MUL,
    JUMP "pow_loop",
    LABEL "pow_out",
    RETURN 1 2,

-- neg
    LABEL "neg",
    LOAD (LB (-1)),
    NEG,
    RETURN 1 1,

-- lt
    LABEL "lt",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    LSS,
    RETURN 1 2,

-- le
    LABEL "le",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    GTR,
    NOT,
    RETURN 1 2,

-- eq
    LABEL "eq",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    EQL,
    RETURN 1 2,

-- ne
    LABEL "ne",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    EQL,
    NOT,
    RETURN 1 2,

-- ge
    LABEL "ge",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    LSS,
    NOT,
    RETURN 1 2,

-- gt
    LABEL "gt",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    GTR,
    RETURN 1 2,

-- and
    LABEL "and",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    AND,
    RETURN 1 2,

-- or
    LABEL "or",
    LOAD (LB (-2)),
    LOAD (LB (-1)),
    OR,
    RETURN 1 2,

-- not
    LABEL "not",
    LOAD (LB (-1)),
    NOT,
    RETURN 1 1,

-- getint
    LABEL "getint",
    GETINT,
    LOAD (LB (-1)),
    STOREI 0,
    RETURN 0 1,

-- putint
    LABEL "putint",
    LOAD (LB (-1)),
    PUTINT,
    RETURN 0 1
    ]
