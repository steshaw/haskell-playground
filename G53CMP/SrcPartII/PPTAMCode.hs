{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		PPTAMCode					     *
*	Purpose:	Simple pretty printer for TAM code		     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Simple pretty printer for TAM Code.

module PPTAMCode (
    ppTAMCode,		-- [TAMInst] -> String
    ppTAMInstState	-- TAMInst -> [MTInt] -> String
) where

-- HMTC module imports
import PPUtilities
import Name
import TAMCode


-- Column widths
cwLbl     = 8
cwOpCode  = 12
cwArgs    = 20
cwState   = 39


------------------------------------------------------------------------------
-- Pretty printing of TAM code (sequence of TAM instructions)
------------------------------------------------------------------------------

-- | Converts TAM code to a nicely laid-out textual representation for
-- display purposes.

ppTAMCode :: [TAMInst] -> String
ppTAMCode is = (foldr (.) id (map (\i -> ppTAMInst i . nl) is)) ""


------------------------------------------------------------------------------
-- Pretty printing of TAM instruction and TAM state (i.e., TAM stack)
------------------------------------------------------------------------------

-- | Converts single TAM instruction and TAM state to a nicely laid-out textual
-- representation for display purposes.

ppTAMInstState :: TAMInst -> [MTInt] -> String
ppTAMInstState i@(LABEL _) _  = (ppTAMInst i . nl) ""
ppTAMInstState i           ns = (ppTAMInst i . ppState ns . nl) ""


ppState :: [MTInt] -> ShowS
ppState [] = showString "[]"
ppState (n:ns) = showString s . ppsAux rw ns
    where
        s  = "[" ++ show n
        rw = cwState - length s

        ppsAux rw []     = showString "]"
        ppsAux rw (n:ns)
            | rw' >= tsrw = showString s . ppsAux rw' ns
            | otherwise   = showString tsr
            where
                s   = ", " ++ show n
                rw' = rw - length s

                -- Representation of truncated state
                tsr = ", ...]"
                tsrw = length tsr


------------------------------------------------------------------------------
-- Pretty printing of TAM instruction
------------------------------------------------------------------------------

-- | Converts single TAM instruction to a nicely laid-out textual
-- representation for display purposes.

ppTAMInst :: TAMInst -> ShowS
ppTAMInst (LABEL l) = showString l . showString ":"
ppTAMInst i         = spcs cwLbl . pptiAux i
    where
        pptiAux (LOADL n)    = ppOA "LOADL"    (show n)
        pptiAux (LOAD a)     = ppOA "LOAD"     (fmtAddr a)
        pptiAux (LOADA a)    = ppOA "LOADA"    (fmtAddr a)
        pptiAux (LOADI d)    = ppOA "LOADI"    (show d)
        pptiAux (STORE a)    = ppOA "STORE"    (fmtAddr a)
        pptiAux (STOREI d)   = ppOA "STOREI"   (show d)
        pptiAux (POP m n)    = ppOA "POP"      (show m ++ " " ++ show n)
        pptiAux ADD	     = ppOA "ADD"      ""
        pptiAux SUB	     = ppOA "SUB"      ""
        pptiAux MUL	     = ppOA "MUL"      ""
        pptiAux DIV	     = ppOA "DIV"      ""
        pptiAux NEG	     = ppOA "NEG"      ""
        pptiAux LSS	     = ppOA "LSS"      ""
        pptiAux EQL	     = ppOA "EQL"      ""
        pptiAux GTR	     = ppOA "GTR"      ""
        pptiAux AND	     = ppOA "AND"      ""
        pptiAux OR	     = ppOA "OR"       ""
        pptiAux NOT	     = ppOA "NOT"      ""
        pptiAux (JUMP l)     = ppOA "JUMP"     l
        pptiAux (JUMPIFZ l)  = ppOA "JUMPIFZ"  l
        pptiAux (JUMPIFNZ l) = ppOA "JUMPIFNZ" l
        pptiAux (CALL l)     = ppOA "CALL"     l
        pptiAux (RETURN m n) = ppOA "RETURN"   (show m ++ " " ++ show n)
        pptiAux PUTINT	     = ppOA "PUTINT"   ""
        pptiAux PUTCHR	     = ppOA "PUTCHR"   ""
        pptiAux GETINT	     = ppOA "GETINT"   ""
        pptiAux GETCHR	     = ppOA "GETCHR"   ""
        pptiAux HALT	     = ppOA "HALT"     ""

        ppOA oc args = leftJust cwOpCode oc . leftJust cwArgs args


fmtAddr :: Addr -> String
fmtAddr (SB d) = "[SB "   ++ fmtDisp d ++ "]"
fmtAddr (LB d) = "[LB "   ++ fmtDisp d ++ "]"
fmtAddr (ST d) = "[ST "   ++ fmtDisp d ++ "]"


fmtDisp :: MTInt -> String
fmtDisp d | d >= 0    = "+ " ++ show d
fmtDisp d | d < 0     = "- " ++ show (abs d)
