{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		TypeChcker					     *
*	Purpose:	MiniTriangle Type Checker			     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | MiniTriangle Type Checker. 

module TypeChecker (
    typeCheck,		-- :: A.AST -> D MTIR
    testTypeChecker	-- :: String -> IO ()
) where


-- Standard library imports
import Maybe (fromJust)
import Monad (zipWithM, unless)

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import Type
import Symbol
import Env
import MTStdEnv
import qualified AST as A
import MTIR
import PPMTIR
import Parser (parse)

-- | Type checks a complete MiniTriangle program in the standard environment 
-- and reports any errors. Hence a computation in the diagnostics monad 'D'.
-- Additionally, translates the program into the type-annotated intermediate
-- representation MTIR. Additionally, implicit dereferencing at the source
-- level is eliminated by inserting explicit dereferencing operations.

typeCheck :: A.AST -> D MTIR
typeCheck (A.AST {A.astCmd = c}) = do
    c' <- tcCommand mtStdEnv c
    return (MTIR {mtirCmd = c'}) 


-- See "MTTypeSystem.txt" for explanation of the concepts and principles
-- behind this type checker, along with the typing rules that define the
-- MiniTriangle type system which this type checker implements.
-- In particular, comments like "T-IF", "env(x) = s" and "env |- e : t"
-- refers to those typing rules.

-- Type checks a command.
tcCommand :: Env -> A.Command -> D Command
-- T-ASSIGN
tcCommand env (A.CmdAssign {A.caVar = x, A.caVal = e, A.cmdSrcPos = sp}) = do
    (x', s) <- varType env x				-- env(x) = s
    (e', t) <- tcExpression env e 			-- env |- e : t
    require (s <: Sink t) sp				-- s <: Sink t
	    (if not (s <: Sink SomeType) then
	         "Variable \"" ++ A.evVar x ++ "\" is not assignable."
             else
                 "Incompatible types: attempting to assign a value of type \""
                 ++ show t ++ "\" to a variable of type \"" ++ show s ++ "\"")
    return (CmdAssign {caVar = x', caVal = e', cmdSrcPos = sp})
-- T-CALL
tcCommand env (A.CmdCall {A.ccProc = p, A.ccArgs = es, A.cmdSrcPos = sp}) = do
    (p', ts, _) <- tcExpArrow env p (length es)		-- env |- p : ts -> _
    es'         <- mapM (agrees env) (zip es ts)	-- env |- es ? ts
    return (CmdCall {ccProc = p', ccArgs = es', cmdSrcPos = sp})
-- T-SEQ (generalized to sequence of any length)
tcCommand env (A.CmdSeq {A.csCmds = cs, A.cmdSrcPos = sp}) = do
    cs' <- mapM (tcCommand env) cs			-- env |- cs
    return (CmdSeq {csCmds = cs', cmdSrcPos = sp})
-- T-IF
-- We know the list of conditions and then-branches will contain at least
-- one pair as the MiniTriangle syntax dictates that the first if and then
-- is not optional.
tcCommand env (A.CmdIf {A.ciCondThens = (e1, c1) : ecs, A.ciMbElse = mbc2,
                        A.cmdSrcPos=sp}) = do
    -- To do! Need to process ecs and mbc2 properly!
    let c2 = fromJust mbc2	-- To fix! Wrong! c2 may not be there!
    e1'	<- tcExpBoolean env e1				-- env |- e1 : Booean
    c1' <- tcCommand env c1				-- env |- c1
    c2' <- tcCommand env c2				-- env |- c2
    return (CmdIf {ciCond = e1', ciThen = c1', ciElse = c2', cmdSrcPos = sp})
-- T-WHILE
tcCommand env (A.CmdWhile {A.cwCond = e, A.cwBody = c, A.cmdSrcPos = sp}) = do
    e' <- tcExpBoolean env e				-- Env |- e : Boolean
    c' <- tcCommand env c				-- Env |- c
    return (CmdWhile {cwCond = e', cwBody = c', cmdSrcPos = sp})
-- T-LETCONST and T-LETVAR
tcCommand env (A.CmdLet {A.clDecls = ds, A.clBody = c, A.cmdSrcPos = sp}) = do
    (ds', env') <- tcDeclarations (openMinScope env) ds	-- Env  |- ds : Env'
    c'          <- tcCommand env' c			-- Env' |- c
    return (CmdLet {clDecls = ds', clBody = c', cmdSrcPos = sp})


tcDeclarations :: Env -> [A.Declaration] -> D ([Declaration], Env)
tcDeclarations env [] = return ([], env)
tcDeclarations env (A.DeclConst {A.dcConst = x, A.dcVal = e, A.dcType = t,
                                 A.declSrcPos=sp} : ds) = do
    t' <- elaborate env t
    (e', s) <- tcExpression env e		-- Env |- e : s
    require (s <: t') sp			-- s <: t'
            ("Incompatible types: attempting to define a constant of type \""
             ++ show t' ++ "\" as a value of type \"" ++ show s ++ "\"")
    let (err, env', tms) = enterIntTermSym x (Source t') sp env
    require (not err) sp (redeclMsg x env)
    (ds', env'') <- tcDeclarations env' ds	-- Env,x:Source t |- ds:Env''
    return (DeclConst {dcConst = tms, dcVal = e'} : ds', env'')
tcDeclarations env (A.DeclVar {A.dvVar=x, A.dvType=t, A.dvMbVal = Nothing,
                               A.declSrcPos=sp} : ds) = do
    t' <- elaborate env t
    let (err, env', tms) = enterIntTermSym x (Ref t') sp env
    require (not err) sp (redeclMsg x env)
    (ds', env'')  <- tcDeclarations env' ds	-- Env,x:Ref t' |-> ds:Env''
    return (DeclVar {dvVar = tms, dvMbVal = Nothing} : ds', env'')
tcDeclarations env (A.DeclVar {A.dvVar=x, A.dvType=t, A.dvMbVal = Just e,
                               A.declSrcPos=sp} : ds) = do
    t' <- elaborate env t
    (e', s) <- tcExpression env e		-- Env |- e : s
    require (s <: t') sp			-- s <: t'
            ("Incompatible types: attempt to initialize a variable of type \""
             ++ show t' ++ "\" using a value of type \"" ++ show s ++ "\"")
    let (err, env', tms) = enterIntTermSym x (Ref t') sp env
    require (not err) sp (redeclMsg x env)
    (ds', env'')  <- tcDeclarations env' ds	-- Env,x:Ref t' |-> ds:Env''
    return (DeclVar {dvVar = tms, dvMbVal = Just e'} : ds', env'')


redeclMsg :: Name -> Env -> String
redeclMsg n env =
    "Identifier \"" ++ n ++ "\" redeclared; already declared at " ++ show sp
    where
        sp = srcPos (snd (lookupTermSym n env))


elaborate :: Env -> A.TypeDenoter -> D Type
elaborate env (A.TDBaseType {A.tdbtName = t, A.tdSrcPos = sp}) = do
    let (err, tps) = lookupTypeSym t env
    require (not err) sp ("undefined type \"" ++ t ++ "\"")
    return (tpsType tps)


-- Type checks an expression. Returns the type of the expression in addition
-- to the MTIR representation of the expression.
tcExpression :: Env -> A.Expression -> D (Expression, Type)
-- T-LITINT
tcExpression env (A.ExpLitInt {A.eliVal = n, A.expSrcPos = sp}) =
    if representableAsMTInt n then
        return (ExpLitInt {eliVal    = fromInteger n,
                           expType   = Integer, 
                           expSrcPos = sp},
                Integer)
     else do
        emitErrD sp ("Integer literal " ++ show n ++ " outside the range of "
                      ++ "representable MiniTriangle integers.")
        return (ExpLitInt {eliVal    = 0, expType   = Integer, expSrcPos = sp},
                Integer)

-- T-DEREFVAR and T-VAR
tcExpression env x@(A.ExpVar {A.expSrcPos = sp}) = do
    (x', s) <- varType env x				-- Env(x) = s
    if s <: Source AllTypes then do			-- s <: Source _
        let t = derefType s
        return (ExpDeref {edArg = x', expType = t, expSrcPos = sp}, t)
     else						-- s /<: Source _
        return (x', s)
-- T-APP
tcExpression env (A.ExpApp {A.eaFun = f, A.eaArgs = es, A.expSrcPos = sp}) = do
    (f', ts, t) <- tcExpArrow env f (length es)		-- Env |- f : ts -> t
    es'         <- mapM (agrees env) (zip es ts)	-- Env |- es ? ts
    return (ExpApp {eaFun = f', eaArgs = es', expType = t, expSrcPos = sp}, t)


-- Type-checks an expression and ensures that its type is compatible with
-- 'Boolean'.
tcExpBoolean :: Env -> A.Expression -> D Expression
tcExpBoolean env e = do
    (e', t) <- tcExpression env e
    require (t <: Boolean) (srcPos e) ("Expected Boolean, got " ++ show t)
    return e'


-- Type-checks an expression and ensures that its type is 'Arrow Ts T'
-- and that the arity (number of arguments) is as expected.
tcExpArrow :: Env -> A.Expression -> Int -> D (Expression, [Type], Type)
tcExpArrow env e n = do
    (e', t) <- tcExpression env e
    case t of
        Arrow ts t -> do
            require (length ts == n) (srcPos e)
                    ("Bad arity: expected " ++ show (length ts)
                     ++ " arguments, got " ++ show n)
            return (e', ensureArity n ts, t)
        SomeType -> do
            return (e', ensureArity n [], SomeType)
        _ -> do
            emitErrD (srcPos e) "Not a function or procedure"
            return (e', ensureArity n [], SomeType)
    where
        ensureArity n ts = take n (ts ++ repeat AllTypes)


-- Type-checks an actual parameter of a procedure/function call and then
-- checks if its type agrees with the type of the formal parameter.
-- Returns MTIR version of the actual parameter.
agrees :: Env -> (A.Expression, Type) -> D Expression
agrees env (x@A.ExpVar {}, t) | isRefType t = do
    (x', s) <- varType env x				-- Env(x) = s
    require (s <: t) (srcPos x) 
            ("Expected a type compatible with \"" ++ show t
             ++ "\", got \"" ++ show s ++ "\"")
    return x'
agrees env (e, t) = do
    (e', s) <- tcExpression env e			-- Env |- e : s
    require (s <: t) (srcPos e)
            ("Expected a type compatible with \"" ++ show t
             ++ "\", got \"" ++ show s ++ "\"")
    return e'


-- Looks up the type of a variable. Returns MTIR representation of the
-- variable along with its type. Reports an error if not found.
varType :: Env -> A.Expression -> D (Expression, Type)
varType env (A.ExpVar {A.evVar = x, A.expSrcPos = sp}) = do
    let (err, tms) = lookupTermSym x env
    require (not err) sp ("Identifier \"" ++ x ++ "\" not declared")
    let t = tmsType tms
    case tms of
        ExtTermSym {etmsVal = v} ->
            case v of
                ESVBool b ->
                    return
                        (ExpLitBool {elbVal = b, expType = t, expSrcPos = sp},
                         t)
                ESVInt  n ->
                    return
                        (ExpLitInt {eliVal = n, expType = t, expSrcPos = sp},
                         t)
                ESVChar c ->
                    return
                        (ExpLitChar {elcVal = c, expType = t, expSrcPos = sp},
                         t)
                ESVLbl  l ->
                    return
                        (ExpExtRef {eerVal = l, expType = t, expSrcPos = sp},
                         t)
        IntTermSym {} ->
            return (ExpVar {evVar = tms, expType = t, expSrcPos = sp}, t)
varType _ _ = tcErr "varType" "Variable expression expected"


-- Report an error unless the condition is true.
require :: Bool -> SrcPos -> String -> D ()
require p sp m = unless p (emitErrD sp m)


-- Check if an Integer is representable as a MiniTriangle integer.
representableAsMTInt :: Integer -> Bool
representableAsMTInt n =
    toInteger (minBound :: MTInt) <= n && n <= toInteger (maxBound :: MTInt)


-- | Test utility. Attempts to parse and then type check the given string
-- input, and, if successful, pretty-prints the resulting MTIR representation.
testTypeChecker :: String -> IO ()
testTypeChecker s = do
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) (snd result)
    putStrLn ""
    case fst result of
        Just mtir -> do
                         putStrLn "MTIR:"
                         putStrLn (ppMTIR mtir)
        Nothing -> putStrLn "Parsing and typechecking produced no result."
    putStrLn ""
    where
        result :: (Maybe MTIR, [DMsg])
        result = runD (parseCheck s)

        parseCheck s = do
            ast <- parse s
            failIfErrorsD
            mtir <- typeCheck ast
            return mtir


tcErr :: String -> String -> a
tcErr = internalError "TypeChecker"
