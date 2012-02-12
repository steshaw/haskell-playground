-- *************************************************************
-- *
-- * Trivial eXpression Language (TXL) to C compiler
-- *
-- * Example for G53CMP, lectures 2 and 3, October 2011
-- *
-- *************************************************************

module Main where

import Char
import System

type Id = String

----------------------------------------------------------------
-- Token type
----------------------------------------------------------------

data Token = T_Int Int
           | T_Id Id
           | T_Plus
           | T_Minus
           | T_Times
           | T_Divide
           | T_LeftPar
           | T_RightPar
           | T_Equal
           | T_Let
           | T_In
           deriving (Eq, Show) 


----------------------------------------------------------------
-- Abstract Syntax Tree
----------------------------------------------------------------

data BinOp = Plus | Minus | Times | Divide deriving Show

data Exp = LitInt   Int
         | Var      Id
         | BinOpApp BinOp Exp Exp
         | Let      Id Exp Exp
         deriving Show

----------------------------------------------------------------
-- Scanner
----------------------------------------------------------------

scanner :: [Char] -> [Token]
-- End of input
scanner []          = []
-- Drop white space and comments
scanner (' '  : cs) = scanner cs
scanner ('\n' : cs) = scanner cs
scanner ('\t' : cs) = scanner cs
scanner ('!'  : cs) = scanner (dropWhile (/='\n') cs)
-- Scan single character tokens
scanner ('+' : cs) = T_Plus     : scanner cs
scanner ('-' : cs) = T_Minus    : scanner cs
scanner ('*' : cs) = T_Times    : scanner cs
scanner ('/' : cs) = T_Divide   : scanner cs
scanner ('(' : cs) = T_LeftPar  : scanner cs
scanner (')' : cs) = T_RightPar : scanner cs
scanner ('=' : cs) = T_Equal    : scanner cs
-- Scan literal integers, identifiers, and keywords
scanner (c : cs) | isDigit c =
                       T_Int (read (c : takeWhile isDigit cs))
                       : scanner (dropWhile isDigit cs)
                 | isAlpha c =
                       mkIdOrKwd (c : takeWhile isAlphaNum cs)
                       : scanner (dropWhile isAlphaNum cs)
                 | otherwise = error "Illegal character!"
    where
        mkIdOrKwd "let" = T_Let
        mkIdOrKwd "in"  = T_In
        mkIdOrKwd cs    = T_Id cs

----------------------------------------------------------------
-- Parser
----------------------------------------------------------------

-- Example of a *Recursive Descent* parser.

-- The basic idea is this. For each non-terminal in the CFG for
-- the language, there is a corresponding parse function having
-- the signature
-- 
-- <token sequence> -> (<abstract syntax tree>,<token sequence>)
-- 
-- Its task is to determine if a *prefix* of the given token
-- sequence can be derived from the corresponding non-terminal. 
-- If that is the case, then it returns an abstract syntax tree
-- representing that prefix and reflecting its syntactic
-- structure, along with the sequence of tokens remaining
-- *after* that prefix. If not, the parse function fails
-- signalling a syntax error. It follows that with this setup,
-- it must be possible to determine what *should* follow at any
-- particular point by looking only at the next (or next few)
-- tokens.
-- 
-- Note that the TXL grammar given during the lecture is *left
-- recursive*. Recursive descent parsers cannot handle left
-- recursive grammars, so the grammar first has to be
-- transformed into a suitable form. This is why the
-- correspondence between the non-terminal "exp",  as given
-- during the lecture, and the corresponding parse function
-- "parseExp" is less direct that the correspondence between the
-- non-terminal "prim-exp" and the corresponding parse function
-- "parsePrimExp". These points will be discussed in depth later
-- in the course.
-- 
-- Finally, note that programming is extremely naive from a
-- functional programming perspective. The same pattern is
-- repeated again and again. If done properly, one would
-- abstract over the recurring patterns by introducing
-- combinators and/or monads.

parser :: [Token] -> Exp
parser ts = if null ts' then e else error "Syntax error!"
    where
        (e, ts') = parseExp ts

parseExp :: [Token] -> (Exp, [Token])
parseExp ts =
    let
        (e, ts') = parsePrimExp ts
    in
        parseExpRec e ts'

parseExpRec :: Exp -> [Token] -> (Exp, [Token])
parseExpRec e [] = (e, [])
parseExpRec e1 tts@(op : ts)
    | op == T_Plus   = parseExpRec (BinOpApp Plus e1 e2) ts'
    | op == T_Minus  = parseExpRec (BinOpApp Minus e1 e2) ts'
    | op == T_Times  = parseExpRec (BinOpApp Times e1 e2) ts'
    | op == T_Divide = parseExpRec (BinOpApp Divide e1 e2) ts'
    | otherwise      = (e1, tts)
    where
        (e2, ts') = parsePrimExp ts

parsePrimExp :: [Token] -> (Exp, [Token])
parsePrimExp (T_Int n : ts) = (LitInt n, ts)
parsePrimExp (T_Id  i : ts) = (Var i, ts)
parsePrimExp (T_LeftPar : ts) =
    let
        (e, ts') = parseExp ts
    in
        case ts' of
	    (T_RightPar : ts'') -> (e, ts'')
            _                   -> error "Expected \")\"!"
parsePrimExp (T_Let : ts) =
    case ts of
        (T_Id i : T_Equal : ts') ->
            let
                (e1, ts1) = parseExp ts'
            in
                case ts1 of
	            (T_In : ts1') ->
		        let
			    (e2, ts2) = parseExp ts1'
                        in
			    (Let i e1 e2, ts2)
                    _ -> error "Expected \"in\"!"
        _ -> error "Expected identifier followed by \"=\"!"
parsePrimExp _ = error "Expected a \"prim-exp\"!"


----------------------------------------------------------------
-- Pretty printing of expressions
----------------------------------------------------------------

ppExp :: Exp -> String
ppExp e = ppExpAux 0 e ""

ppExpAux :: Int -> Exp -> ShowS
ppExpAux n (LitInt x) = indent n . showString (show x) . nl
ppExpAux n (Var i)    = indent n . showString i . nl
ppExpAux n (BinOpApp op e1 e2) =
    indent n . showString "BinOpApp" . nl
    . ppExpAux (n+1) e1
    . indent (n+1) . showString (show op) . nl
    . ppExpAux (n+1) e2
ppExpAux n (Let i e1 e2) =
    indent n . showString "Let" . nl
    . indent (n+1) . showString i . nl
    . ppExpAux (n+1) e1
    . ppExpAux (n+1) e2

----------------------------------------------------------------
-- Contextual analysis
----------------------------------------------------------------

-- Returns a list of error messages.
-- I.e. somewhat better error reporting than in the rest of the
-- compiler! Only checks that all used variables have been
-- defined.

checkExp :: Exp -> [String]
checkExp e = checkExpAux [] e

checkExpAux :: [Id] -> Exp -> [String]
checkExpAux env (LitInt _) = []
checkExpAux env (Var i) | i `elem` env = []
                        | otherwise    = ["Variable "
					   ++ i
 					   ++ " not defined."]
checkExpAux env (BinOpApp _ e1 e2) =
    checkExpAux env e1 ++ checkExpAux env e2
checkExpAux env (Let i e1 e2) =
    checkExpAux env e1 ++ checkExpAux (i:env) e2


----------------------------------------------------------------
-- Compile to C code
----------------------------------------------------------------

-- The main problem of generating C code from TXL is that there
-- is nothing in C corresponding to let-expressions. There are
-- blocks in C, which can have local variables, but blocks are
-- statements, not expressions. Moreover, ignoring that, the
-- scope rules would be wrong anyway. E.g. in a C block like
-- 
--    { int x = x + 1; ... }
-- 
-- The second "x" refers to the first one, which is currently
-- being defined, rather than to an "x" from an outer scope,
-- which would have been required if we were to implement TXL
-- let expressions in terms of C blocks.
-- 
-- So what do we do? The idea is this. We rename each let-bound
-- variable, giving it a new name distinct from all other
-- variable names. Obviously, that means renaming variables at
-- the definition site, as well as at every use site, taking
-- the TXL scope rules into account. Once that is done, we
-- "lift out" all let expressions into a linear sequence of
-- global variable definitions. Since each variable name now is
-- unique, we do not have to worry about scope rules or name
-- clashes at this point, but to facilitate the mapping to
-- valid C code, we should ensure that each definition precedes
-- its first use.

-- First phase: convert to a list of variable definitions, one
-- for each let, and a single expression to be printed.

liftLets :: Exp -> ([(Id, Exp)], Exp)
liftLets e = (ds, e')
    where
        (_, ds, e') = liftLetsAux 0 e


-- The first argument "n" is a name supply used for generating
-- new variable names. It is just an integer that is
-- incremented each time a new name is needed, and that value
-- is then converted into a digit sequence. Since TXL variable
-- names have to start with a letter, there is no risk that
-- generated names clashes with user-supplied names. The
-- updated name supply is returned as part of the result of
-- each call to "liftLetsAux" making it possible to thread it
-- into the next call, thus ensuring globally unique names. The
-- astute reader may recognise a pattern that fruitfully could
-- be captured using a state monad ...

liftLetsAux :: Int -> Exp -> (Int, [(Id, Exp)], Exp)
liftLetsAux n e@(LitInt _) = (n, [], e)
liftLetsAux n e@(Var _)    = (n, [], e)
liftLetsAux n (BinOpApp op e1 e2) =
    let
        (n',  ds1, e1') = liftLetsAux n e1
        (n'', ds2, e2') = liftLetsAux n' e2
    in
        (n'', ds1 ++ ds2, BinOpApp op e1' e2')
liftLetsAux n (Let i e1 e2) =
    let
        (n', ds1, e1') = liftLetsAux n e1
        freshName      = show n	  -- String of digits:
                                  -- cannot clash with ids.
        (n'', ds2, e2') = liftLetsAux (n'+1)
                                      (subst freshName i e2)
    in
        (n'', ds1 ++ [(freshName, e1')] ++ ds2, e2')


subst :: Id -> Id -> Exp -> Exp
subst _  _ e@(LitInt _) = e
subst i1 i e@(Var i') | i /= i'   = e
                      | otherwise = (Var i1)
subst i1 i (BinOpApp op e1 e2) =
    BinOpApp op (subst i1 i e1) (subst i1 i e2)
subst i1 i e@(Let i' e1 e2)
    | i /= i'   = Let i' (subst i1 i e1) (subst i1 i e2)
    | otherwise = Let i' (subst i1 i e1) e2


-- Second phase: print the list of definitions and the
-- expression as syntactically valid C code.
printAsC :: ([(Id, Exp)], Exp) -> String
printAsC (ds, e) = printAsCAux ""
    where
        printAsCAux = printPreamble
		      . printCDecls ds
		      . indent 1 . showString "printf(\"%d\\n\", "
                      . printCExp e
                      . showString ");" . nl
                      . indent 1 . showString "return 0;" . nl
                      . printEpilogue

printPreamble :: ShowS
printPreamble = showString "#include <stdio.h>" . nl
                . nl
                . showString "int main(int argc, char* argv[]) {"
                . nl

printEpilogue :: ShowS
printEpilogue = showString "}" . nl

printCExp :: Exp -> ShowS
printCExp (LitInt x) = showString (show x)
printCExp (Var i)   = printCVar i
printCExp (BinOpApp op e1 e2) =
    showChar '('
    . printCExp e1
    . spc
    . printCOp op
    . spc
    . printCExp e2
    . showChar ')'
printCExp (Let _ _ _) =
    error "Internal error: should not encounter let here!"

printCDecls :: [(Id, Exp)] -> ShowS
printCDecls [] = id
printCDecls ((i, e) : ds) =
    indent 1
    . showString "int "
    . printCVar i
    . spc
    . showChar '='
    . spc
    . printCExp e
    . showChar ';'
    . nl
    . printCDecls ds

printCVar :: Id -> ShowS
printCVar i = showChar 'v' . showString i

printCOp :: BinOp -> ShowS
printCOp Plus   = showChar '+'
printCOp Minus  = showChar '-'
printCOp Times  = showChar '*'
printCOp Divide = showChar '/'


----------------------------------------------------------------
-- Utilities
----------------------------------------------------------------

nl  = showChar '\n'

spc = showChar ' '

indent n s = iterate spc s !! (n * 2)

----------------------------------------------------------------
-- Main
----------------------------------------------------------------

compiler :: [Char] -> [Char]
compiler txlCode = if null contErrs
                   then cCode
                   else error (unlines contErrs)
    where
	tokens   = scanner txlCode
        ast      = parser tokens
        contErrs = checkExp ast
        defsExp  = liftLets ast
        cCode    = printAsC defsExp        

-- Usage:
--     txl2c file.txl	Compile "file.txl" and write result
--			in "file.c"
--     txl2c		Read input from standard input and write
--			result to standard output.
--			(Could be confusing!)

main = do
    args <- getArgs
    case args of
        [] -> do
            input <- getContents
            putStr (compiler input)
        (infile:_) -> do
	    input <- readFile infile
	    let outfile = takeWhile (/='.') infile ++ ".c"
            writeFile outfile (compiler input)
