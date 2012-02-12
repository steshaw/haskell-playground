{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Diagnostics					     *
*	Purpose:	Diagnostic messages and computations (monad)	     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Diagnostic messages and computations (monad). The module provides
-- a datatype for representing diagnostic messages, tagged with
-- source-code position and severity level (error, warning, information).
-- It also provides an abstraction for diagnostic computations, where
-- a computation has the ability to emit diagnostic messages and, possibly,
-- fail. Finally, it provides a utility function for reporting internal
-- compiler errors in a standardized way.

module Diagnostics (
    -- * Diagnostic messages
    DMsg (..), 		-- Not abstract. Instances: HasSrcPos.
    DMLvl (..),		-- Not abstract. Instances: Eq, Ord.
    mkInfoMsg,		-- :: SrcPos -> String -> DMsg
    mkWngMsg,		-- :: SrcPos -> String -> DMsg
    mkErrMsg,		-- :: SrcPos -> String -> DMsg
    dmIsInfo,		-- :: DMsg -> Bool
    dmIsWng,		-- :: DMsg -> Bool
    dmIsErr,		-- :: DMsg -> Bool
    dmCmpLvlPos,	-- :: DMsg -> DMsg -> Ordering
    ppDMsg,		-- :: DMsg -> String

    -- * Diagnostic computations
    D,			-- Abstract. Instances: Monad.
    emitInfoD,		-- :: SrcPos -> String -> D ()
    emitWngD,		-- :: SrcPos -> String -> D ()
    emitErrD,		-- :: SrcPos -> String -> D ()
    failD,		-- :: SrcPos -> String -> D a
    failNoReasonD,	-- :: D ()
    failIfErrorsD,	-- :: D ()
    stopD,		-- :: D a
    runD,		-- :: D a -> (Maybe a, [DMsg])

    -- * Reporting internal errors
    internalError	-- :: String -> String -> String -> a
) where

-- Standard library imports
import List (sortBy)

-- HMTC module imports
import SrcPos


------------------------------------------------------------------------------
-- Diagnostic messages
------------------------------------------------------------------------------

-- | Representation of diagnostic messages
data DMsg = DMsg {
         dmLvl    :: DMLvl,		-- ^ Severity level
	 dmSrcPos :: SrcPos,		-- ^ Associated source position
	 dmTxt    :: String		-- ^ Message text
     }


-- | Severity levels
data DMLvl = DMLInfo	-- ^ Information (least severe)
           | DMLWng 	-- ^ Warning
           | DMLErr 	-- ^ Error (most severe)
           deriving (Eq, Ord)


instance HasSrcPos DMsg where
    srcPos = dmSrcPos


-- | Constructs information message.
mkInfoMsg :: SrcPos -> String -> DMsg
mkInfoMsg sp msg = DMsg {
                       dmLvl    = DMLInfo,
		       dmSrcPos = sp,
		       dmTxt    = msg
                   }

-- | Constructs warning message.
mkWngMsg :: SrcPos -> String -> DMsg
mkWngMsg sp msg = DMsg {
                      dmLvl    = DMLWng,
		      dmSrcPos = sp,
		      dmTxt    = msg
                  }


-- | Constructs error message.
mkErrMsg :: SrcPos -> String -> DMsg
mkErrMsg sp msg = DMsg {
                      dmLvl    = DMLErr,
		      dmSrcPos = sp,
		      dmTxt    = msg
                  }


-- | Checks if information message.
dmIsInfo :: DMsg -> Bool
dmIsInfo (DMsg {dmLvl = DMLInfo}) = True
dmIsInfo _                        = False


-- | Checks if warning message.
dmIsWng :: DMsg -> Bool
dmIsWng (DMsg {dmLvl = DMLWng}) = True
dmIsWng _                       = False


-- | Checks if error message.
dmIsErr :: DMsg -> Bool
dmIsErr (DMsg {dmLvl = DMLErr}) = True
dmIsErr _                       = False


-- | Comparison function for ordering messages first by severity, then
-- by source position.
dmCmpLvlPos :: DMsg -> DMsg -> Ordering
dmCmpLvlPos (DMsg {dmLvl = lvl1, dmSrcPos = sp1})
            (DMsg {dmLvl = lvl2, dmSrcPos = sp2}) =
    case compare lvl1 lvl2 of
        LT -> GT	-- Unsevere messages last
        GT -> LT	-- Severe messages first
        EQ -> compare sp1 sp2


-- | Formats a diagnostic message for printing.
ppDMsg :: DMsg -> String
ppDMsg (DMsg {dmLvl = lvl, dmSrcPos = sp, dmTxt = msg}) =
    kind ++ srcPosTxt sp ++ ":\n" ++ msg ++ "\n"
    where
        kind = case lvl of
                   DMLInfo -> "Information"
                   DMLWng  -> "Warning"
                   DMLErr  -> "Error"

        srcPosTxt NoSrcPos = ""
        srcPosTxt sp       = " at " ++ show sp


------------------------------------------------------------------------------
-- Diagnostic computations
------------------------------------------------------------------------------

-- | Diagnostic computation. A computation with diagnostic output can
-- succeed or fail, and additionally yields a list of diagnostic messages.
newtype D a = D ([DMsg] -> (Maybe a, [DMsg]))


unD :: D a -> ([DMsg] -> (Maybe a, [DMsg]))
unD (D x) = x


instance Monad D where
    return a = D (\dms -> (Just a, dms))

    d >>= f = D (\dms ->
                     case unD d dms of
                         (Nothing, dms') -> (Nothing, dms')
		         (Just a, dms')  -> unD (f a) dms')


-- Note on the use of '$':
-- '$' is the explicit function application operator in Haskell.
-- It is useful because it binds less tightly than normal function
-- application. This allows parentheses around (potentially large)
-- function arguments to be omitted. For example, below, instead
-- of writing
--
--     D (\dms -> ...)
--
-- I write
--
--     D $ \dms -> ...
--
-- This also allows the "..." to be spread out over subsequent lines
-- without worrying about the final ")".


-- | Emits an information message.
emitInfoD :: SrcPos -> String -> D ()
emitInfoD sp msg = D $ \dms ->
    (Just (), mkInfoMsg sp msg : dms)


-- | Emits a warning message.
emitWngD :: SrcPos -> String -> D ()
emitWngD sp msg = D $ \dms ->
    (Just (), mkWngMsg sp msg : dms)


-- | Emits an error message.
emitErrD :: SrcPos -> String -> D ()
emitErrD sp msg = D $ \dms ->
    (Just (), mkErrMsg sp msg : dms)


-- | Emits an error message and fails.
failD :: SrcPos -> String -> D a
failD sp msg = D $ \dms ->
    (Nothing, mkErrMsg sp msg : dms)


-- | Fails without giving any specific reason.
failNoReasonD :: D ()
failNoReasonD = D $ \dms ->
    (Nothing, dmUtterFailure : dms)
    where
        dmUtterFailure = mkErrMsg NoSrcPos
                                  "Failure, unknown reason"


-- | Fails if there has been errors thus far
failIfErrorsD :: D ()
failIfErrorsD = D $ \dms ->
    if any dmIsErr dms then
        (Nothing, dms)
    else
        (Just (), dms)


-- | Forces a stop, e.g. after some user-specified pass.
stopD :: D a
stopD = D $ \dms ->
    (Nothing, dms)


-- | Runs a diagnostic computation. Returns:
--
-- (1) Result of the computation, if any.
--
-- (2) Sorted list of diagnostic messages.

runD :: D a -> (Maybe a, [DMsg])
runD d = (ma, sortBy dmCmpLvlPos dms)
    where
        (ma, dms) = unD d []


------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

-- | Signals an internal compiler error.
-- Call with module name, name of function, and error message
-- to report internal errors; i.e., things that should not happen.

internalError :: String -> String -> String -> a
internalError m f msg =
    error ("[Internal Error] " ++ m ++ "." ++ f ++ ": " ++ msg )
