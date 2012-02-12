-- **********************************************************
-- *
-- * Less Trivial eXpression Language (LTXL) Parser and
-- * Identification
-- * 
-- * Illustration of implementation of identification phase.
-- *
-- * Example for G53CMP, lecture 8, October 2011
-- *
-- **********************************************************

{
module Main where

import Char
import System

}

-------------------------------------------------------------
-- Parser
-------------------------------------------------------------

-- Happy grammar with semantic actions for building an AST.

%name parser
%tokentype { Token }

%token
    litint	{ T_LitInt $$ }
    ident       { T_Id $$ }
    '||'	{ T_Or }
    '&&'	{ T_And }
    '<'		{ T_Less }
    '=='	{ T_Equal }
    '>'         { T_Greater }
    '+'		{ T_Plus }
    '-'		{ T_Minus }
    '*'		{ T_Times }
    '/'		{ T_Divide }
    '\\'	{ T_Not }
    '('		{ T_LeftPar }
    ')'		{ T_RightPar }
    if		{ T_If }
    then        { T_Then }
    else        { T_Else }
    let		{ T_Let }
    in		{ T_In }
    int		{ T_IntType }
    bool	{ T_BoolType }
    '='		{ T_Is }
    ';'         { T_SemiColon }

%right in else
%left '||'
%left '&&'
%left '<' '==' '>'
%left '+' '-'
%left '*' '/'

%%

ltxl_program : exp		{ $1 }

exp : exp '||' exp              { BinOpApp Or $1 $3 }
    | exp '&&' exp		{ BinOpApp And $1 $3 }
    | exp '<' exp		{ BinOpApp Less $1 $3 }
    | exp '==' exp		{ BinOpApp Equal $1 $3 }
    | exp '>' exp		{ BinOpApp Greater $1 $3 }
    | exp '+' exp		{ BinOpApp Plus $1 $3 }
    | exp '-' exp		{ BinOpApp Minus $1 $3 }
    | exp '*' exp		{ BinOpApp Times $1 $3 }
    | exp '/' exp		{ BinOpApp Divide $1 $3 }
    | pexp                      { $1 }

pexp : litint			{ LitInt $1 }
     | ident	        	{ Var $1 () }
     | '\\' pexp                { UnOpApp Not $2 }
     | '-' pexp                 { UnOpApp Neg $2 }
     | if exp then exp else exp { If $2 $4 $6 }
     | '(' exp ')'       	{ $2 }
     | let decls in exp	        { Let $2 $4 } 

decls : decl ';' decls          { $1 : $3 }
      |decl                     { [$1] }

decl : type ident '=' exp	{ ($2, $1, $4) }

type : int			{ IntType }
     | bool			{ BoolType }

{
-- Haskell code for defining token type, AST, scanner,
-- top-level functions. More or less verbatim from the
-- original TXL example (lectures 1 + 2).


type Id = String

-------------------------------------------------------------
-- Token type
-------------------------------------------------------------

data Token = T_LitInt Int
           | T_Id Id
           | T_Or
	   | T_And
           | T_Less
           | T_Equal
           | T_Greater
           | T_Plus
           | T_Minus
           | T_Times
           | T_Divide
           | T_Not
           | T_LeftPar
           | T_RightPar
           | T_Let
           | T_In
           | T_IntType
           | T_BoolType
           | T_Is
           | T_SemiColon
           | T_If
           | T_Then
           | T_Else
           deriving Show 


-------------------------------------------------------------
-- Abstract Syntax Tree with Annotations
-------------------------------------------------------------

-- This representation of the abstract syntax tree allows
-- applied identifier occurrences to be annotated with
-- information.
--
-- The parser initially creates a tree where the applied
-- identifiers are unannotated, i.e. the tree is of type
-- "Exp ()". In contrats, each variable declaration is
-- associated with basic attributes of the variable
-- that are readily available from the source program,
-- here its type.
--
-- The goal of the identification phase is to create
-- a tree where each applied occurrence is annotated with
-- the attributes of the corresponding declaration
-- according to the scope rules. The result of identification
-- is thus a tree of type "Exp Attr" for some type "Attr",
-- e.g. a tuple of scope level and type.

-- Representation of types.
-- UnknownType is used when a type is unknown due to a
-- type or other contextual error.
data Type = IntType | BoolType | UnknownType deriving Show

data UnOp = Not | Neg deriving Show

data BinOp = Or
           | And 
           | Less 
           | Equal 
           | Greater
           | Plus 
           | Minus 
           | Times 
           | Divide
           deriving Show

data Exp a = LitInt   Int
           | Var      Id a
           | UnOpApp  UnOp (Exp a)
           | BinOpApp BinOp (Exp a) (Exp a)
           | If       (Exp a) (Exp a) (Exp a)
           | Let      [(Id, Type, Exp a)] (Exp a)
           deriving Show


-------------------------------------------------------------
-- Scanner
-------------------------------------------------------------

scanner :: [Char] -> [Token]
-- End of input
scanner []          = []
-- Drop white space and comments
scanner (' '  : cs) = scanner cs
scanner ('\n' : cs) = scanner cs
scanner ('!'  : cs) = scanner (dropWhile (/='\n') cs)
-- Scan graphic tokens
scanner ('|'  : '|' : cs) = T_Or        : scanner cs
scanner ('&'  : '&' : cs) = T_And       : scanner cs
scanner ('<'  : cs)       = T_Less      : scanner cs
scanner ('='  : '=' : cs) = T_Equal     : scanner cs
scanner ('>'  : cs)       = T_Greater   : scanner cs
scanner ('+'  : cs)       = T_Plus      : scanner cs
scanner ('-'  : cs)       = T_Minus     : scanner cs
scanner ('*'  : cs)       = T_Times     : scanner cs
scanner ('/'  : cs)       = T_Divide    : scanner cs
scanner ('\\' : cs)       = T_Not       : scanner cs
scanner ('('  : cs)       = T_LeftPar   : scanner cs
scanner (')'  : cs)       = T_RightPar  : scanner cs
scanner ('='  : cs)       = T_Is        : scanner cs
scanner (';'  : cs)       = T_SemiColon : scanner cs
-- Scan literal integers, identifiers, and keywords
scanner (c : cs) | isDigit c =
                       T_LitInt (read (c :
                                       takeWhile isDigit cs))
                       : scanner (dropWhile isDigit cs)
                 | isAlpha c =
                       mkIdOrKwd (c :
                                  takeWhile isAlphaNum cs)
                       : scanner (dropWhile isAlphaNum cs)
                 | otherwise = error "Illegal character!"
    where
        mkIdOrKwd "if"   = T_If
        mkIdOrKwd "then" = T_Then
        mkIdOrKwd "else" = T_Else
        mkIdOrKwd "let"  = T_Let
        mkIdOrKwd "in"   = T_In
        mkIdOrKwd "int"  = T_IntType
        mkIdOrKwd "bool" = T_BoolType
        mkIdOrKwd cs     = T_Id cs


-------------------------------------------------------------
-- Pretty printing of expressions
-------------------------------------------------------------

ppExp :: Show a => Exp a -> String
ppExp e = ppExpAux 0 e ""

ppExpAux :: Show a => Int -> Exp a -> ShowS
ppExpAux n (LitInt x) = indent n . showString (show x) . nl
ppExpAux n (Var i a)  =
    indent n
    . showString i 
    . showString " ["
    . shows a
    . showChar ']'
    . nl
ppExpAux n (UnOpApp op e) =
    indent n . showString "UnOpApp" . nl
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e
ppExpAux n (BinOpApp op e1 e2) =
    indent n . showString "BinOpApp" . nl
    . ppExpAux (n+1) e1
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e2
ppExpAux n (If e1 e2 e3) =
    indent n . showString "If" . nl
    . ppExpAux (n+1) e1
    . ppExpAux (n+1) e2
    . ppExpAux (n+1) e3
ppExpAux n (Let ds e) =
    indent n . showString "Let" . nl
    . ppDefs   (n+1) ds
    . ppExpAux (n+1) e

ppDefs n [] = id
ppDefs n ((i, t, e) : ds) =
    indent n . showString i . nl
    . indent (n+1) . shows t . nl
    . ppExpAux (n+1) e
    . ppDefs n ds


-------------------------------------------------------------
-- Environment (Identification table/Symbol table)
-------------------------------------------------------------

-- For the sake of simplicity, we use a linear list to
-- represent the environment. By prepending new declarations
-- to the list, and searching from the beginning, it is
-- ensured that we will always find an identifier in the
-- closest containing scope.
--
-- There is no need for "open scope" and "close scope"
-- opertaions. As all data structures in a pure functional
-- language, this one is *persistent*, meaning that it never
-- gets destructively  updated. Thus all we need to do to
-- close a scope is to make sure we have a reference to an
-- appropriate old version of the structure around and
-- revert to that version.


type ErrorMsg = String

-- Variable attributes are the scope level of the declaration
-- and the type of the variable
type Attr = (Int, Type)

type Env = [(Id, Attr)] 

emptyEnv :: Env
emptyEnv = []

-- Enters a variable at the given scope level and of the
-- given type into the environment. A check is first
-- performed so that the no other variable with the same
-- name has been defined at the same scope level. If not,
-- the new variable is entered into the table. Otherwise
-- an error message is returned.

enterVar :: Id -> Int -> Type -> Env -> Either Env ErrorMsg
enterVar i l t env
    | not (isDefined i l env) = Left ((i,(l,t)) : env)
    | otherwise               = Right errMsg
    where
        isDefined i l [] = False
        isDefined i l ((i',(l',_)) : env)
            | l < l'    = error "Should not happen! (Bad level.)"
            | l > l'    = False
            | i == i'   = True
            | otherwise = isDefined i l env

        errMsg = "Variable " ++ i
                 ++ " is alreday defined at this level ("
                 ++ show l ++ ")."

-- Looks up a variable and returns its attributes if found.
-- Otherwise returns an error message.

lookupVar :: Id -> Env -> Either Attr ErrorMsg
lookupVar i [] = Right ("Variable " ++ i
                        ++ " is not defined.")
lookupVar i ((i',a) : env)
    | i == i'   = Left a
    | otherwise = lookupVar i env


-------------------------------------------------------------
-- Identification
-------------------------------------------------------------

-- The goal of the identification phase is to create
-- a tree where each applied occurrence is annotated with
-- the attributes of the corresponding declaration
-- according to the scope rules. The result of identification
-- is thus a tree of type "Exp Attr" for the type "Attr",
-- here a tuple of scope level and type.
--
-- In addition, a list of error messages is returned.
--
-- Written for clarity, not efficiency.


identification :: Exp () -> (Exp Attr, [ErrorMsg])
identification e = identAux 0 emptyEnv e

identAux :: Int -> Env -> Exp () -> (Exp Attr, [ErrorMsg])
identAux l env (LitInt n) =
    (LitInt n, [])
identAux l env (Var i _) =
    case lookupVar i env of
        Left a  -> (Var i a, [])
        Right m -> (Var i (0, UnknownType), [m])
identAux l env (UnOpApp op e) =
    (UnOpApp op e', ms)
    where
        (e', ms) = identAux l env e
identAux l env (BinOpApp op e1 e2) =
    (BinOpApp op e1' e2', ms1 ++ ms2)   
    where
        (e1', ms1) = identAux l env e1
        (e2', ms2) = identAux l env e2
identAux l env (If e1 e2 e3) =
    (If e1' e2' e3', ms1 ++ ms2 ++ ms3)
    where
        (e1', ms1) = identAux l env e1
        (e2', ms2) = identAux l env e2
        (e3', ms3) = identAux l env e3
identAux l env (Let ds e) =
    (Let ds' e', ms1 ++ ms2)
    where
        l' = l + 1
        (ds', env', ms1) = identDefs l' env ds
        (e', ms2)        = identAux l' env' e

        identDefs l env [] = ([], env, [])
        identDefs l env ((i, t, e) : ds) =
            ((i, t, e') : ds', env'', ms1 ++ ms2 ++ ms3)
            where
                (e', ms1) = identAux l env e -- i NOT in scope!	    
                (env', ms2) =
                    case enterVar i l t env of
                       Left env' -> (env', [])
                       Right m   -> (env,  [m])
                (ds', env'', ms3) =
                    identDefs l env' ds    -- i IN scope!


-------------------------------------------------------------
-- Utilities
-------------------------------------------------------------

indent n = showString (take (2 * n) (repeat ' '))

nl  = showChar '\n'

spc = showChar ' '

happyError :: [Token] -> a
happyError _ = error "Parse error"


-------------------------------------------------------------
-- Test utilities
-------------------------------------------------------------

-- Reads, parses, prints and runs identification on test
-- programs.

test file =
    do
        prog <- readFile file
        let tree = (parser . scanner) prog
        putStrLn "Program before identification:"
        putStrLn ""
        putStrLn (ppExp tree)
        putStrLn ""
        let (tree', msgs) = identification tree
        putStrLn "Program after identification:"
        putStrLn ""
        putStrLn (ppExp tree')
        putStrLn ""
        putStrLn "Error Messages:"
        mapM_ putStrLn msgs


-------------------------------------------------------------
-- Main
-------------------------------------------------------------

-- Usage:
--     ltxl file.ltxl	Parse "file.ltxl", perform
--                      identification, and write result
--                      to standard output.
--     ltxl		Read input from standard input,
--                      perform identification, and write
--                      result to standard output.
--			(Could be confusing!)

main = do
    args <- getArgs
    prog <- if null args
             then getContents
             else readFile (head args)
    let tree = (parser . scanner) prog
    putStrLn "Program before identification:"
    putStrLn ""
    putStrLn (ppExp tree)
    putStrLn ""
    let (tree', msgs) = identification tree
    putStrLn "Program after identification:"
    putStrLn ""
    putStrLn (ppExp tree')
    putStrLn ""
    putStrLn "Error Messages:"
    mapM_ putStrLn msgs

}
