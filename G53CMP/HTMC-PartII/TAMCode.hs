{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		TAMCode						     *
*	Purpose:	Triangle Abstract Machine (TAM) Code		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Triangle Abstract Machine (TAM) Code.

module TAMCode (
    MTInt(..),		-- TAM integer type
    Addr(..),		-- Address
    TAMInst(..)
) where

-- HMTC module imports
import Name


-- To do:
-- * Be more precise about sizes for various fields (e.g., it is unrealistic
--   for POP and RETURN to have two MTInt arguments.)
-- * Introduce newtypes (using newtype deriving for simplicity?) to
--   get get proper type checking for this.

type Word32 = Int	-- Temporary: Assume Haskell integer type is 32-bit. 
type MTInt = Word32	-- This does not belong here: It's a MT characteristic!


-- | TAM stack addresses
data Addr
    = SB MTInt		-- ^ SB (Stack base) + displacement:         [SB + d]
    | LB MTInt		-- ^ LB (Local Base) + displacement:         [LB + d]
    | ST MTInt		-- ^ ST (Stack Top) + displacement:          [ST + d]
    deriving (Eq, Show)

-- | TAM instruction type.
data TAMInst
    -- Label
    = LABEL Name	-- ^ Symbolic location (pseudo instructio)

    -- Load, store, and stack manipulation
    | LOADL  MTInt	-- ^ Push literal integer onto stack
    | LOAD   Addr	-- ^ Push contents at addres onto stack
    | LOADA  Addr 	-- ^ Push address onto stack
    | LOADI  MTInt	-- ^ Load indirectly, addr = top stack elem. + displ.
    | STORE  Addr	-- ^ Pop a value from stack and store at address
    | STOREI MTInt      -- ^ Store indirectly, adrr = top stack elem. + displ.
    | POP   MTInt MTInt	-- ^ POP m n: pop n values below top m values
    
    -- Aritmetic operations
    | ADD		-- ^ [b, a, ...] => [a + b, ...]
    | SUB		-- ^ [b, a, ...] => [a - b, ...]
    | MUL		-- ^ [b, a, ...] => [a * b, ...]
    | DIV		-- ^ [b, a, ...] => [a / b, ...]
    | NEG		-- ^ [a, ...]    => [-a, ...]

    -- Comparison & logical ops: false = 0, true = 1 (as arg., anything /= 0)
    | LSS		-- ^ [b, a, ...] => [a < b, ...]
    | EQL		-- ^ [b, a, ...] => [a == b, ...]
    | GTR		-- ^ [b, a, ...] => [a > b, ...]
    | AND		-- ^ [b, a, ...] => [a && b, ...]
    | OR		-- ^ [b, a, ...] => [a || b, ...]
    | NOT		-- ^ [a, ...]    => [!a, ...]

    -- Control transfer
    | JUMP     Name	-- ^ Jump unconditionally 
    | JUMPIFZ  Name	-- ^ Pop top value off stack, jump if zero (false)
    | JUMPIFNZ Name	-- ^ Pop top value off stack, jump if not zero (true)
    | CALL     Name	-- ^ Call subroutine
    | RETURN   Int Int	-- ^ RETURN m n: result size m, arguments size n.

    -- I/O
    | PUTINT		-- ^ Pop and print top stack element to terminal
    | PUTCHR		-- ^ Pop and print top stack element interp. as char.
    | GETINT		-- ^ Read an integer and push onto stack
    | GETCHR		-- ^ Read a character and push onto stack

    -- TAM Control
    | HALT              -- ^ Stop TAM
    deriving (Eq, Show)
