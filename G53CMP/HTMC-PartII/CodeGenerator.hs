{-
******************************************************************************
*				   H M T C				     *
*									     *
*	Module:		CodeGenerator					     *
*	Purpose:	Generate Triangle Abstract Machine (TAM) code from   *
*                       MiniTriangle INtermediate Representation (MTIR)	     *
*	Authors:	Henrik Nilsson					     *
*									     *
*                 Copyright (c) Henrik Nilsson, 2006 - 2011                  *
*									     *
******************************************************************************
-}

-- | TAM Code Generator.

module CodeGenerator (
    genCode	-- :: MTIR -> D [TAMInst]
) where

-- Standard library imports
import Monad (when)
import Char (isDigit)
import Array

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import Symbol
import MTIR
import TAMCode
import CodeGenMonad


------------------------------------------------------------------------------
-- Code generator environment
------------------------------------------------------------------------------

-- Maps internal term-level symbols either to a displacement (with the
-- proper base (SB, LB, or via static links) being determined by scope
-- level of symbol w.r.t. current scope level) or to a label (Name, for
-- local procedures and functions).
--
-- At present, local procedures or functions is not supported, and
-- thus everything is at the global level (addressing w.r.t. SB).
--
-- Note that the value of an *external* symbol is part of the symbol.
-- The value of internal symbols only become known *during* code generation.
-- That is why internal symbol do not have a value field, and why we need
-- an environment here to keep track of the mapping from internal symbols to
-- their values.
--
-- List used for simplicity. Could use a more efficient structure.
-- As symbols are used as keys (i.e., name + scope level), and symbols are
-- distinct within a scope, there is no need to use a structure and
-- lookup strategy that takes account of shadowing.

type CGEnv = [(TermSym, IntSymVal)]

data IntSymVal
    = ISVDisp MTInt	-- Displacment w.r.t. base of activation record
    | ISVLbl Name	-- Label for entry point of local function/procedure.

emptyCGEnv = []

lookupISV :: TermSym -> CGEnv -> IntSymVal
lookupISV tms env =
    case lookup tms env of
        Just isv -> isv
        Nothing  -> cgErr "lookupISV"
                           ("Lookup of \"" ++ show tms ++ "\" failed!")


------------------------------------------------------------------------------
-- Code generation functions
------------------------------------------------------------------------------

-- | TAM Code Generator.

-- Diagnostics computation as one may want to emit information or warnings.
-- However, there shouldn't really be any further errors to report.
genCode :: MTIR -> D [TAMInst]
genCode mtir = do
    let (_, code, _) = runCG (run mtir)
    return code


-- Type synonum for the TAM code generation monad
type TAMCG a = CG TAMInst () a


-- Generate code to run a complete program
run :: MTIR -> TAMCG ()
run (MTIR { mtirCmd = c}) = do
    execute emptyCGEnv 0 c
    emit HALT


-- Generate code to execute a command.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The command
--
execute :: CGEnv -> MTInt -> Command -> TAMCG ()
execute env n (CmdAssign {caVar = v, caVal = e}) =
    case v of
        (ExpVar {evVar = tms}) -> do
            evaluate env n e
            emit (STORE (addr env tms))
        _ ->
            cgErr "execute"
                  "Assignment to non-manifest veriable not supported at \
                  \present"
execute env n (CmdCall {ccProc = p, ccArgs = as}) =
    case p of
        ExpExtRef {eerVal = l} -> do
            evaluateSeq env n as
            emit (CALL l)
        _ ->
            cgErr "evaluate"
                  "Call to unknown functions not supported at present"
execute env n (CmdSeq {csCmds = cs}) = executeSeq env n cs
execute env n (CmdIf {ciCond = e, ciThen = c1, ciElse = c2}) = do
    lblElse <- newName
    lblOver <- newName
    evaluate env n e
    emit (JUMPIFZ lblElse)
    execute env n c1
    emit (JUMP lblOver)
    emit (LABEL lblElse)
    execute env n c2
    emit (LABEL lblOver)
execute env n (CmdWhile {cwCond = e, cwBody = c}) = do
    lblLoop <- newName
    lblCond <- newName
    emit (JUMP lblCond)
    emit (LABEL lblLoop)
    execute env n c
    emit (LABEL lblCond)
    evaluate env n e
    emit (JUMPIFNZ lblLoop)
execute env n (CmdLet {clDecls = ds, clBody = c}) = do
    (env', n') <- elaborateSeq env n ds
    execute env' n' c
    emit (POP 0 (n' - n))


-- Generate code to execute a sequence of commands.
-- Invariant: Stack depth unchanged.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The commands
--
executeSeq :: CGEnv -> MTInt -> [Command] -> TAMCG ()
executeSeq env n []     = return ()
executeSeq env n (c:cs) = do
    execute env n c
    executeSeq env n cs


-- Generate code to evaluate an expression.
-- Result is left on the top of the stack.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The expression
--
-- Returns: New stack depth
--
evaluate :: CGEnv -> MTInt -> Expression -> TAMCG MTInt
evaluate env n (ExpLitBool {elbVal = b}) = do
    emit (LOADL (tamRepBool b))
    return (n + 1)
evaluate env n (ExpLitInt {eliVal = v}) = do
    emit (LOADL v)
    return (n + 1)
evaluate env n (ExpLitChar {elcVal = c}) = do
    emit (LOADL (tamRepChar c))
    return (n + 1)
evaluate env n (ExpExtRef {eerVal = l}) =
    cgErr "evaluate"
          "Pushing code segment addresses onto the TAM stack not supported \
          \at present"
evaluate env n (ExpVar {evVar = tms}) = do
    emit (LOADA (addr env tms))
    return (n + 1) 
evaluate env n (ExpDeref {edArg = a}) =
    case a of
        (ExpVar {evVar = tms}) -> do
            emit (LOAD (addr env tms))
            return (n + 1)
        _ ->
            cgErr "evaluate" 
                  "Dereferencing of non-manifest variable not supported at \
                  \present!"
evaluate env n (ExpApp {eaFun = f, eaArgs = as}) =
    case f of
        ExpExtRef {eerVal = l} -> do
            evaluateSeq env n as
            emit (CALL l)
            return (n + 1)
        _ ->
            cgErr "evaluate"
                  "Call to unknown functions not supported at present"


-- Generate code to evaluate a sequence of expressions.
-- The results are left on the top of the stack (last one at the very top).
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The expressions
--
-- Returns: New stack depth
--
evaluateSeq :: CGEnv -> MTInt -> [Expression] -> TAMCG MTInt
evaluateSeq env n []     = return n
evaluateSeq env n (e:es) = do
    n'  <- evaluate env n e
    n'' <- evaluateSeq env n' es
    return n''


-- Elaborate declaration and generate code to compute (initial) value (if any).
-- Storage for the variable is allocated on the top of the stack.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The declaration
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth

elaborate :: CGEnv -> MTInt -> Declaration -> TAMCG (CGEnv, MTInt)
elaborate env n (DeclConst {dcConst = tms, dcVal = e}) = do
    n' <- evaluate env n e
    return ((tms, ISVDisp n) : env, n')
elaborate env n (DeclVar {dvVar = tms, dvMbVal = Nothing}) = do
    emit (LOADL 0)	-- Reserve space
    return ((tms, ISVDisp n) : env, n + 1)
elaborate env n (DeclVar {dvVar = tms, dvMbVal = Just e}) = do
    n' <- evaluate env n e
    return ((tms, ISVDisp n) : env, n')


-- Elaborate a sequence of declarations.
-- Storage for the variables are allocated on the top of the stack.
-- Arguments:
--
-- (1) Enviornment
--
-- (2) Current stack depth w.r.t. base of current activation record
--
-- (3) The declarations
--
-- Returns: 
--
-- (1) New environment
--
-- (2) New stack depth

elaborateSeq :: CGEnv -> MTInt -> [Declaration] -> TAMCG (CGEnv, MTInt)
elaborateSeq env n []     = return (env, n)
elaborateSeq env n (d:ds) = do
    (env', n')   <- elaborate env n d
    (env'', n'') <- elaborateSeq env' n' ds
    return (env'', n'')


------------------------------------------------------------------------------
-- Address of symbol
------------------------------------------------------------------------------

-- Only for stack addresses for now. This will have to be refactored to
-- support addresses into the code segment (result should be Either Addr Name?)
-- following static links (actually need to generate code?).
-- Also assumed that ext. refs. do not refer to integer/Boolean/char constants:
-- should have been eliminated earlier. (But maybe rethink that too:
-- in principle easy to support; same code as for LitInt etc.

addr :: CGEnv -> TermSym -> Addr
addr env (ExtTermSym {}) =
    cgErr "addr"
          "Pushing code segment addresses onto the TAM stack not supported at \
          \present"
addr env tms@(IntTermSym {}) =
    case lookupISV tms env of
        ISVDisp d -> SB d	-- Only relative stack base for now.
        ISVLbl l  -> cgErr "addr"
                           "Pushing code segment addresses onto the TAM stack \
                           \not supported at present"


------------------------------------------------------------------------------
-- TAM data representation
------------------------------------------------------------------------------

tamRepBool :: Bool -> MTInt
tamRepBool False = 0
tamRepBool True  = 1


tamRepChar :: Char-> MTInt
tamRepChar c = fromEnum c


------------------------------------------------------------------------------
-- Internal error reporting
------------------------------------------------------------------------------

cgErr :: String -> String -> a
cgErr = internalError "CodeGenerator"
