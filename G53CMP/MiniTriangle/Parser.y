--
-- Found at http://hpaste.org/steps/52842
--

-- ***************************************************************************
-- *                               H M T C                                   *
-- *                                                                         *
-- *    Module:         Parser                                               *
-- *    Purpose:        MiniTriangle parser                                  *
-- *    Authors:        Henrik Nilsson                                       *
-- *                                                                         *
-- *               Copyright (c) Henrik Nilsson, 2006 - 2011                 *
-- *                                                                         *
-- ***************************************************************************

{
-- | MiniTriangle parser

module Parser (
    parse,              -- :: String -> D AST
    testParser          -- :: String -> IO ()
) where

-- HMTC module imports
import SrcPos
import Diagnostics
import Name
import Token
import AST
import PPAST
import ParseMonad
import Scanner

}

----------------------------------------------------------------
-- Parser
----------------------------------------------------------------

-- Happy grammar with semantic actions for building an AST.
-- Convention: Terminals are either written in upper case or within
-- single quotes. Non-terminals are written in lower case.

%name parseAux

%monad { P } { >>= } { return }

%lexer { scanner } { (EOF, _) }

-- The terminal symbols are tokens paired with a source code position.
%tokentype { (Token, SrcPos) }

-- The semantic values of constant terminal symbols are the associated
-- source code position. The semantic value of terminal symbols that
-- carry additional information (like identifiers) is the token and
-- source code position pair itself.
%token
    '('         { (LPar, $$) }
    ')'         { (RPar, $$) }
    ','     { (Comma, $$) }
    ';'         { (Semicol, $$) }
    ':'         { (Colon, $$) }
    ':='        { (ColEq, $$) }
    '='         { (Equals, $$) }
    '?'     { (QMark, $$) }
    BEGIN       { (Begin, $$) }
    CONST       { (Const, $$) }
    DO          { (Do, $$) }
    ELSE        { (Else, $$) }
    END         { (End, $$) }
    IF          { (If, $$) }
    IN          { (In, $$) }
    LET         { (Let, $$) }
    THEN        { (Then, $$) }
    VAR         { (Var, $$) }
    WHILE       { (While, $$) }
  REPEAT  { (Repeat, $$) }
  UNTIL   { (Until, $$) }
    LITINT      { (LitInt {}, _) }
    ID      { (Id {}, _) }
    '+'         { (Op {opName="+"},   _) }
    '-'         { (Op {opName="-"},   _) }
    '*'         { (Op {opName="*"},   _) }
    '/'         { (Op {opName="/"},   _) }
    '^'         { (Op {opName="^"},   _) }
    '<'         { (Op {opName="<"},   _) }
    '<='        { (Op {opName="<="},  _) }
    '=='        { (Op {opName="=="},  _) }
    '!='        { (Op {opName="!="},  _) }
    '>='        { (Op {opName=">="},  _) }
    '>'         { (Op {opName=">"},   _) }
    '&&'        { (Op {opName="&&"},  _) }
    '||'        { (Op {opName="||"},  _) }
    '!'         { (Op {opName="!"},   _) }

%right '?'
%left '||'
%left '&&'
%nonassoc '<' '<=' '==' '!=' '>=' '>'
%left '+' '-'
%left '*' '/'
%right '^'

%%

program :: { AST }
program : command       { AST $1 }


commands :: { [Command] }
commands : command              { [$1] } 
         | command ';' commands { $1 : $3 }


command :: { Command }
command
    : var_expression ':=' expression
        { CmdAssign {caVar = $1, caVal=$3, cmdSrcPos = srcPos $1} }
    | var_expression '(' expressions ')'
        { CmdCall {ccProc = $1, ccArgs = $3, cmdSrcPos = srcPos $1} }
    | IF expression THEN command ELSE command
        { CmdIf {ciCond = $2, ciThen = $4, ciElse = $6, cmdSrcPos = $1} }
    | WHILE expression DO command
        { CmdWhile {cwCond = $2, cwBody = $4, cmdSrcPos = $1} }
    | REPEAT command UNTIL expression
        { CmdRepeat {crBody = $2, crUntil = $4, cmdSrcPos = $1} }
    | LET declarations IN command
        { CmdLet {clDecls = $2, clBody = $4, cmdSrcPos = $1} }
    | BEGIN commands END
        { if length $2 == 1 then
        head $2
    else
        CmdSeq {csCmds = $2, cmdSrcPos = srcPos $2}
  }

expressions :: { [Expression] }
expressions : expression { [$1] }
            | expression ',' expressions { $1 : $3 }


-- The terminal associated with a precedence declaration has to occur
-- *literally* in a rule if precedence declarations are to be taken into
-- account. That means a lot of repetitive productions. To simplify a bit,
-- non-terminals for *classes* of operators having the same precedence
-- and associativity are introduced, along with one expression production
-- for each precedence level. The latter production has to be annotated with
-- an explicit precedence declaration.
--
-- (Alternatively, the scanner could classify operators into classes. But it
-- was decided to handle precedence and associativity completely within the
-- parser.)

expression :: { Expression }
expression
    : primary_expression
        { $1 }
    | expression opclass_disjunctive expression %prec '||'
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_conjunctive expression %prec '&&'
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_relational expression %prec '=='
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_additive expression %prec '+'
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_multiplicative expression %prec '*'
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression opclass_exponential expression %prec '^'
  { ExpApp {eaFun     = $2,
                  eaArgs    = [$1,$3],
                  expSrcPos = srcPos $1} }
    | expression '?' expression ':' expression %prec '?'
  { ExpTer {etCond     = $1,
                  etTrue    = $3,
                  etFalse   = $5,
                  expSrcPos = srcPos $1} }


primary_expression :: { Expression }
    : LITINT
        { ExpLitInt {eliVal = tspLIVal $1, expSrcPos = tspSrcPos $1} }
    | var_expression
        { $1 }
    | opclass_unary primary_expression
  { ExpApp {eaFun = $1, eaArgs = [$2], expSrcPos = srcPos $1}}
    | '(' expression ')'
        { $2 }


-- Variables being assigned to, procedures being called, and functions being
-- applied (currently only operators) are restricted to be denoted by simple
-- identifiers or operators in the current MiniTriangle grammar. However, a
-- indicated by the non-terminal VarExpression, this could be generalized.
-- For example, if arrays were introduced, another form of variable would be
-- an array element which might be denoted by, say, x[i]. This would thus be
-- another form of variable expression. For that reason, variables, procedures,
-- and functions are represented as expressions in the abstract syntax.
-- denoted by operators) are also represented as expressions.

var_expression :: { Expression }
    : ID { ExpVar {evVar = tspIdName $1, expSrcPos = tspSrcPos $1} }


opclass_disjunctive :: { Expression }
    : '||' { mkExpVarBinOp $1 }

opclass_conjunctive :: { Expression }
    : '&&' { mkExpVarBinOp $1 }

opclass_relational :: { Expression }
    : relational_op { mkExpVarBinOp $1 }

relational_op
    : '<'  { $1 }
    | '<='  { $1 }
    | '=='  { $1 }
    | '!='  { $1 }
    | '>='  { $1 }
    | '>'  { $1 }

opclass_additive :: { Expression }
    : additive_op { mkExpVarBinOp $1 }

additive_op
    : '+'  { $1 }
    | '-'  { $1 }

opclass_multiplicative :: { Expression }
    : multiplicative_op { mkExpVarBinOp $1 }

multiplicative_op
    : '*'  { $1 }
    | '/'  { $1 }

opclass_exponential :: { Expression }
    : '^' { mkExpVarBinOp $1 }

opclass_unary :: { Expression }
    : unary_op { mkExpVarUnOp $1 }

unary_op
    : '!' { $1 }
    | '-'  { $1 }


declarations :: { [Declaration] }
declarations
    : declaration
  { [$1] } 
    | declaration ';' declarations
  { $1 : $3 }


declaration :: { Declaration }
declaration
    : CONST ID ':' type_denoter '=' expression
  { DeclConst {dcConst = tspIdName $2, dcType = $4, dcVal = $6,
         declSrcPos = $1} }
    | VAR ID ':' type_denoter
        { DeclVar {dvVar = tspIdName $2, dvType = $4, dvMbVal = Nothing,
          declSrcPos = $1} }
    | VAR ID ':' type_denoter ':=' expression
        { DeclVar {dvVar = tspIdName $2, dvType = $4, dvMbVal = Just $6,
          declSrcPos = $1} }


type_denoter :: { TypeDenoter }
type_denoter : ID       { TDBaseType {tdbtName = tspIdName $1,
                                      tdSrcPos = tspSrcPos $1} }


{

happyError :: P a
happyError = failP "Parse error"


-- | Parses a MiniTriangle program, building an AST representation of it
-- if successful.

parse :: String -> D AST
parse = runP parseAux


-- Projection functions for pairs of token and source position.

tspSrcPos :: (Token,SrcPos) -> SrcPos
tspSrcPos = snd


tspLIVal :: (Token,SrcPos) -> Integer
tspLIVal (LitInt {liVal = n}, _) = n
tspLIVal _ = parserErr "tspLIVal" "Not a LitInt"


tspIdName :: (Token,SrcPos) -> Name
tspIdName (Id {idName = nm}, _) = nm
tspIdName _ = parserErr "tspIdName" "Not an Id"


tspOpName :: (Token,SrcPos) -> Name
tspOpName (Op {opName = nm}, _) = nm
tspOpName _ = parserErr "tspOpName" "Not an Op"


-- Helper functions for building ASTs.

-- Builds ExpVar from pair of Binary Operator Token and SrcPos.
mkExpVarBinOp :: (Token,SrcPos) -> Expression
mkExpVarBinOp otsp =
    ExpVar {evVar = tspOpName otsp, expSrcPos = tspSrcPos otsp}


-- Builds ExpVar from pair of Unary Operator Token and SrcPos.
-- As a special case, the unary operator "-" is substituted for the name
-- "neg" to avoid confusion with the binary operator "-" later.
mkExpVarUnOp :: (Token,SrcPos) -> Expression
mkExpVarUnOp otsp =
    ExpVar {evVar = nm, expSrcPos = tspSrcPos otsp}
    where
        onm = tspOpName otsp
        nm  = if onm == "-" then "neg" else onm


-- | Test utility. Attempts to parse the given string input and,
-- if successful, pretty-prints the resulting AST.

testParser :: String -> IO ()
testParser s = do
    putStrLn "Diagnostics:"
    mapM_ (putStrLn . ppDMsg) (snd result)
    putStrLn ""
    case fst result of
        Just ast -> do
                        putStrLn "AST:"
                        putStrLn (ppAST ast)
        Nothing -> putStrLn "Parsing produced no result."
    putStrLn ""
    where
        result :: (Maybe AST, [DMsg])
        result = runD (parse s)


parserErr :: String -> String -> a
parserErr = internalError "Parser"

}
