-- ***************************************************************************
-- *									     *
-- *		     Less Trivial eXpression Language (LTXL)		     *
-- *									     *
-- *	Module:		Environment					     *
-- *	Purpose:	Type-checking environment and operations	     *
-- *	Author:		Henrik Nilsson					     *
-- *									     *
-- *           Example for G53CMP, lectures 12 & 13, November 2011           *
-- *									     *
-- ***************************************************************************

module Environment (
    VarAttr,
    Env,	-- Abstract
    glblLvl,    -- :: Int
    initEnv,    -- :: [(Id, Type)] -> [(UnOp, Type)] -> [(BinOp, Type)] -> Env
    enterVar,	-- :: Id -> Int -> Type -> Env -> Either Env ErrorMsg
    lookupVar,	-- :: Id -> Env -> Either VarAttr ErrorMsg
    lookupUO,	-- :: UnOp -> Env -> Type
    lookupBO	-- :: BinOp -> Env -> Type
) where

import List (nub)

import Diagnostics
import AbstractSyntax (Id, UnOp, BinOp)
import Type (Type)


------------------------------------------------------------------------------
-- Environment (Symbol table)
------------------------------------------------------------------------------

-- The environment stores information about entities, in particular their
-- type.
--
-- There are three kinds of entities. Each have a distinct kind of name
-- (used for looking it up), and a distinct set of attributes:
--   * Variables
--        Name: Identifier
--        Attributes: Scope level and type.
--   * Unary operator
--        Name: UnOp
--        Attributes: Type only (the scope level is always the global one).
--   * Binary operator
--        Name: BinOp
--        Attributes: Type only (the scope level is always the global one).
--
--
-- The reason that operators are separated from variables is that the LTXL
-- syntax defines a fixed set of operators (user-defined operators are not
-- supported). For the same reason, we also know that an operator is an entity
-- defined at the global level.
-- 
-- The environment is an Abstract Data Type (ADT) which is achieved by not
-- exporting its constructor from this module. It is split into three parts,
-- one for each kind of entity. For the sake of simplicity, we use a linear
-- association list to represent the environment. By prepending new
-- declarations to the list, and searching from the beginning, it is ensured
-- that we will always find an identifier in the closest containing scope, as
-- specified by the LTXL Scope Rules.


type VarAttr = (Int, Type)

data Env = Env {
               varEnv :: [(Id, VarAttr)],	-- The variable environment
               uoEnv  :: [(UnOp, Type)],	-- The unary operator env.
               boEnv  :: [(BinOp, Type)]	-- Tye binary operator env.
           } 


glblLvl :: Int
glblLvl = 0


-- Creates an initial environment (everything defined at the top level).

initEnv :: [(Id, Type)] -> [(UnOp, Type)] -> [(BinOp, Type)] -> Env
initEnv ve uoe boe =
    if ok then
        Env {
            varEnv = [ (i, (glblLvl, tp)) | (i, tp) <- ve ],
            uoEnv  = uoe,
            boEnv  = boe
        }
    else
        internalError "Bad initial environment: multiply defined entities"
    where
        ok = length is == length (nub is)
             && length uos == length (nub uos)
             && length bos == length (nub bos)
        is  = map fst ve
        uos = map fst uoe
        bos = map fst boe


-- Enters a variable at the given scope level and of the given type into the
-- environment. A check is first performed so that the no other variable with
-- the same name has been defined at the same scope level. If not, the new
-- variable is entered into the table. Otherwise an error message is returned.

enterVar :: Id -> Int -> Type -> Env -> Either Env ErrorMsg
enterVar i l t env@(Env {varEnv = ve})
    | not (isDefined i l ve) = Left (env {varEnv = ((i,(l,t)) : ve)}) 
    | otherwise              = Right errMsg
    where
        isDefined i l [] = False
        isDefined i l ((i',(l',_)) : ve)
            | l < l'  = error "Should not happen! (Bad level.)"
            | l > l'  = False
            | i == i' = True
            | otherwise = isDefined i l ve

        errMsg = "Variable " ++ i
                 ++ " is already defined at this level ("
                 ++ show l ++ ")."


-- Looks up a variable and returns its attributes if found.
-- Otherwise returns an error message.

lookupVar :: Id -> Env -> Either VarAttr ErrorMsg
lookupVar i env = lvRec i (varEnv env)
    where
        lvRec i [] = Right ("Variable " ++ i ++ " is not defined.")
        lvRec i ((i',a) : ve)
            | i == i'   = Left a
            | otherwise = lvRec i ve


-- Looks up a unary operator and returns its type.
-- It should always be found, otherwise there is an error in the compiler.

lookupUO :: UnOp -> Env -> Type
lookupUO uo env =
    case lookup uo (uoEnv env) of
        Just tp -> tp
        Nothing -> internalError ("Unknown unary operator " ++ show uo)


-- Looks up a binary operator and returns its type.
-- It should always be found, otherwise there is an error in the compiler.

lookupBO :: BinOp -> Env -> Type
lookupBO bo env =
    case lookup bo (boEnv env) of
        Just tp -> tp
        Nothing -> internalError ("Unknown unary operator " ++ show bo)
