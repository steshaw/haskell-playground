-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		TypeChecker					     *
-- *	Purpose:	Type checker for LTXL				     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module TypeChecker (typeCheck, tcAux) where

import Diagnostics
import AbstractSyntax
import Type
import Environment
import GlobalEnvironment


------------------------------------------------------------------------------
-- Type Checker
------------------------------------------------------------------------------

-- The type checker checks whether LTXL terms are well-typed. In the process,
-- it also does "identification" as it has to link up definitions and uses
-- of variables.
-- 
-- A "real" type checker might return an AST annotated with type information
-- and information about the scope level of variables etc., which then is used
-- for optimization and code generation. However, to keep things simple, this
-- type checker just checks types and returns the type of a term (if any)
-- along with a list of identification and type errors.
--
-- Important: A term is well-typed if and only if no error messages are
-- returned. Only then can the returned type be "trusted".
--
-- The "type" TpUnknown is returned (along with an error message) when there
-- is a problem and there is no "better" type to return. To avoid one type
-- error causing lots of other type errors, the type "TpUnknown" is treated as
-- "compatible" with *any* other type. This could potentially mask some
-- further real errors, but is preferable to reporting lots of problems when
-- there is only one error.
-- 
-- Written for clarity, not efficiency.


typeCheck :: Exp -> (Type, [ErrorMsg])
typeCheck e = tcAux 0 glblEnv e

tcAux :: Int -> Env -> Exp -> (Type, [ErrorMsg])
tcAux l env (LitInt n)          = (TpInt, [])
tcAux l env (Var i)             =
    case lookupVar i env of
        Left (_, t) -> (t, [])
        Right m     -> (TpUnknown, [m])
tcAux l env (UnOpApp uo e1)     =
    (t2, ms1 ++ ms)
    where
        TpArr t1 t2 = lookupUO uo env	-- Safe if compiler impl. correctly
        (t1', ms1)  = tcAux l env e1
        ms          = if compatible t1 t1' then
                          []
                      else
                          [illTypedOpApp t1 t1']
tcAux l env (BinOpApp bo e1 e2) =
    (t3, ms1 ++ ms2 ++ ms)
    where
        TpArr (TpProd t1 t2) t3 = lookupBO bo env
        (t1', ms1)              = tcAux l env e1
        (t2', ms2)              = tcAux l env e2
        ms                      = (if compatible t1 t1' then
                                       []
                                   else
                                       [illTypedOpApp t1 t1'])      
                                  ++
                                  (if compatible t2 t2' then
                                       []
                                   else
                                       [illTypedOpApp t2 t2'])
tcAux l env (If e1 e2 e3)       =
    (t2, ms1 ++ ms2 ++ ms3 ++ ms)	-- or t3,
					-- or if null ms then t2 else TpUnknown
    where
        (t1, ms1) = tcAux l env e1
        (t2, ms2) = tcAux l env e2
        (t3, ms3) = tcAux l env e3
        ms        = (if compatible t1 TpBool then
                         []
                     else
                         [illTypedCond t1])      
                    ++
                    (if compatible t2 t3 then
                         []
                     else
                         [incompatibleBranches t2 t3])
tcAux l env (Let ds e)          =
    (t, ms1 ++ ms)
    where
        l' = l + 1
        (env', ms1) = tcDefs l' env ds
        (t, ms)     = tcAux l' env' e


-- For simplified LTXL scope rules (a defined variable NOT in scope
-- in further definitions of the same let).
tcDefs :: Int -> Env -> [(Id, Type, Exp)] -> (Env, [ErrorMsg])
tcDefs l env [] = (env, [])
tcDefs l env ((i, t, e) : ds) =
    (env'', ms1 ++ ms2 ++ ms3 ++ ms4)
    where
        (t', ms1)    = tcAux l env e		-- i NOT in scope!
        ms2          = if compatible t t' then [] else [declMismatch t t']
        (env', ms3)  = tcDefs l env ds		-- i NOT in scope!
        (env'', ms4) = case enterVar i l t env' of
                           Left env'' -> (env'', [])
                           Right m    -> (env', [m])                       


------------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------------

-- Type comparison where "TpUnknown" is treated as compatible with any
-- other type to avoid one type error causing further errors.

compatible :: Type -> Type -> Bool
compatible TpUnknown _         = True
compatible _         TpUnknown = True
compatible t1        t2        = t1 == t2


illTypedOpApp :: Type -> Type -> ErrorMsg
illTypedOpApp t1 t2 =
    "Ill-typed operator application: expected type " ++ ppType t1
    ++ ", got type " ++ ppType t2


illTypedCond :: Type -> ErrorMsg
illTypedCond t = "Ill-typed condition: expected bool, got " ++ ppType t


incompatibleBranches :: Type -> Type -> ErrorMsg
incompatibleBranches t1 t2 =
    "Expected same type in both then and else branch, but got types "
    ++ ppType t1 ++ " and " ++ ppType t2


declMismatch :: Type -> Type -> ErrorMsg
declMismatch t1 t2 =
    "Declared type " ++ ppType t1
    ++ " does not match inferred type " ++ ppType t2

