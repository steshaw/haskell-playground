{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		Env						     *
*	Purpose:	Environment (symbol table) with operations	     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | Environment with operations. The module provides an abstract datatype
-- for representing environments (symbol tables), along with operations
-- for creating and extending environments, and looking up symbols.
-- An environment can contain two kinds of symbols:  term-level symbols
-- denoting entities like variables, constants, and procedures, and
-- type-level symbols, denoting types.

-- Note: "mkTopLvlEnv" and "enterTermSym" are the responsible for creating
-- symbols. Creating symbols only in the context of an environment allows
-- scope level assignment to be handled automatically.

module Env (
    -- Environments
    Env,		-- Abstract.
    mkTopLvlEnv,	-- :: [(Name,Type)] -> [(Name,Type,ExtSymVal)] -> Env
    openMajScope,	-- :: Env -> Env
    openMinScope,	-- :: Env -> Env
    enterIntTermSym,	-- :: Name -> Type -> SrcPos -> Env
                        --    -> (Bool, Env, TermSym)
    lookupTypeSym,	-- :: Name -> Env -> (Bool, TypeSym)
    lookupTermSym	-- :: Name -> Env -> (Bool, TermSym)
) where


-- HMTC module imports
import Diagnostics (internalError)
import Name
import SrcPos
import Type
import Symbol


-- | Environment (symbol table). Abstract.

-- An environment is represented by the current scope level,
-- a list of type-level symbols, and a list of term-level symbols.

data Env = Env ScopeLvl [TypeSym] [TermSym]


topScopeLvl :: ScopeLvl
topScopeLvl = (0, 0)


-- | Creates an initial, top-level, environment of external symbols.
-- Arguments:
--
-- (1) List of name and type pairs for the type-level part of the
-- top-level environment; i.e. a /definition/ for each symbol.
-- 
-- (2) List of name and type pairs for the term-level part of the
-- top-levelenvironment; i.e., a /declaration/ for each symbol.
--
-- Returns: A top-level environment.

mkTopLvlEnv :: [(Name,Type)] -> [(Name,Type,ExtSymVal)] -> Env
mkTopLvlEnv tpnts tmnts = Env topScopeLvl tpss tmss
    where
        tpss = [ TypeSym {tpsName = n, tpsType = t, tpsSrcPos = NoSrcPos}
                 | (n,t) <- tpnts
               ]
        tmss = [ ExtTermSym {
                     tmsName = n, 
                     tmsType = t,
                     etmsVal = v
                 }
                 | (n, t, v) <- tmnts
               ]


-- | Opens a new major (e.g. procedure/function) scope level.
openMajScope :: Env -> Env
openMajScope (Env (majl, _) tpss tmss) = Env (majl + 1, 0) tpss tmss


-- | Opens a new minor (e.g. let) scope level.
openMinScope :: Env -> Env
openMinScope (Env (majl, minl) tpss tmss) = Env (majl, minl + 1) tpss tmss


-- | Enters an internal term-level symbol into an environment.
-- Enforces that symbols are uniquely defined at each scope level.
-- Arguments:
--
-- (1) Name of symbol to be entered.
--
-- (2) Type of symbol to be entered.
--
-- (3) Source position of the declaration or definition of symbol to be
--     entered.
-- 
-- (4) The environment to extend.
--
-- Returns:
--
-- (1) Flag indicating redeclaration error.
-- 
-- (2) Extended environment.
-- 
-- (3) Copy of the new symbol.

enterIntTermSym :: Name -> Type -> SrcPos -> Env -> (Bool, Env, TermSym)
enterIntTermSym n t sp (Env l tpss tmss) =
    (redeclared, Env l tpss (tms:tmss), tms)
    where
        tms = IntTermSym {
		  itmsLvl    = l,
		  tmsName    = n,
		  tmsType    = if not redeclared then t else SomeType,
		  itmsSrcPos = sp
              }

        redeclared = redeclaredAux tmss

        redeclaredAux [] = False
        redeclaredAux (ExtTermSym {} : tmss) = redeclaredAux tmss
        redeclaredAux (IntTermSym {itmsLvl = l', tmsName = n'} : tmss)
            | l' < l    = False
            | n' == n   = True
            | otherwise = redeclaredAux tmss


-- | Looks up a type-level symbol.
--  Arguments:
--
-- (1) Name of symbol to lookup.
-- 
-- (2) The environment in which to lookup the symbol.
-- 
-- Returns:
--
-- (1) Flag indicating lookup failure.
--
-- (2) The symbol.
--
-- (Lookup failure is indicated by a boolean flag, as opposed to making the
-- return type a Maybe type, since that is slightly more convenient when
-- writing  the type checker.)

lookupTypeSym :: Name -> Env -> (Bool, TypeSym)
lookupTypeSym n (Env _ tpss _) = ltpsAux tpss
    where
        ltpsAux []                              = (True, dummyTpS)
        ltpsAux (tps : tpss) | tpsName tps == n = (False, tps)
                             | otherwise        = ltpsAux tpss

        dummyTpS = TypeSym {
                       tpsName   = n,
                       tpsType   = SomeType,
                       tpsSrcPos = NoSrcPos
                   }


-- | Looks up a term-level symbol.
-- Later declarations (higher scope levels) shadow earlier ones.
-- Arguments:
--
-- (1) Name of symbol to lookup.
-- 
-- (2) The environment in which to lookup the symbol.
-- 
-- Returns:
--
-- (1) Flag indicating lookup failure.
--
-- (2) The symbol.

-- (Lookup failure is indicated by a boolean flag, as opposed to making the
-- return type a Maybe type, since that is slightly more convenient when
-- writing the type checker.)

lookupTermSym :: Name -> Env -> (Bool, TermSym)
lookupTermSym n (Env _ _ tmss) = ltmsAux tmss
    where
        ltmsAux []                              = (True, dummyTmS)
        ltmsAux (tms : tmss) | tmsName tms == n = (False, tms)
                             | otherwise        = ltmsAux tmss


        dummyTmS = IntTermSym {
		       itmsLvl    = topScopeLvl,
                       tmsName   = n,
                       tmsType   = SomeType,
                       itmsSrcPos = NoSrcPos
                   }

envErr :: String -> String -> a
envErr = internalError "Env"
