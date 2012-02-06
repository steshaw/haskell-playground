--
-- Modified by Steven Shaw.
--

-- Parser for Imp.
-- Developed for use in COMP3610
-- A straightforward, hand-crafted, top-down predictive parser.
-- Clem Baker-Finch

-- This has been modified from the first version, now called Parser1, to
-- make the treatment of the grammar of command sequences more standard.
-- The commentary has also been modified and is now hopefully more
-- helpful for students interested in understanding parsers.

module Parser (parse, parseProg) where

import Scanner
import AbsSyn

-- Check that the next token is the one expected:

check :: Token -> [Token] -> [Token]

check tok []  = error "Unexpected end of input."
check tok (t:ts)
    | tok == t  = ts
    | otherwise = error (unscan tok ++ " expected.\n" ++ unscan t ++ " scanned.")

-- Parse an arithmetic expression.
-- Operator precedence is as usual, represented by the following grammar:

-- expr  ::= expr + term | expr - term | term
-- term ::= term * factor | factor
-- factor ::= num | ident | (expr)

-- Eliminate left recursion and left to factor give:

-- expr ::= term exprOpt
-- exprOpt ::= + term exprOpt | - term exprOpt | <empty>
-- term ::= factor termOpt
-- termOpt ::= * factor exprOpt | <empty>
-- factor ::= num | ident | (expr)

-- Director symbols:

-- first(expr) = first(term) = first(factor) = {NUM, ID, LPAREN}

-- first(exprOpt) = {PLUS, MINUS}
-- follow(exprOpt) = {RPAREN, ...}

-- For the full grammar developed below, I reckon the complete definition
-- is:
-- follow(exprOpt) = {RPAREN, EQUAL, LEQ} U follow(bexpr) U follow(com)
--                 = {RPAREN, EQUAL, LEQ, AND, OR,
--                    THEN, ELSE, DO, RBRACE, SEMICOLON, EOI}

-- first(term) = first(factor) = {NUM, ID, LPAREN}

-- first(termOpt) = {TIMES}
-- follow(termOpt) = follow(exprOpt) U {PLUS, MINUS}

-- first(factor) = {NUM, ID, LPAREN}

expr :: [Token] -> (Aexp, [Token])

expr = exprOpt . term

exprOpt :: (Aexp, [Token]) -> (Aexp, [Token])

exprOpt (t1, PLUS:toks)  = exprOpt (t1 :+: t2, toks')
    where
    (t2, toks') = term toks
exprOpt (t1, MINUS:toks) = exprOpt (t1 :-: t2, toks')
    where
    (t2, toks') = term toks
exprOpt (t, toks)        = (t, toks)

term ::  [Token] -> (Aexp, [Token])

term = termOpt . factor

termOpt :: (Aexp, [Token]) -> (Aexp, [Token])

termOpt (f1, TIMES:toks) = termOpt (f1 :*: f2, toks')
    where
    (f2, toks') = factor toks
termOpt (f, toks)        = (f, toks)

factor ::  [Token] -> (Aexp, [Token])

factor (NUM n:toks)  = (Num n, toks)
factor (ID x:toks)   = (Var x, toks)
factor (LPAREN:toks) = (e,toks'')
    where
    (e, toks') = expr toks
    toks''     = check RPAREN toks'
factor (t:toks)      = error ("Unexpected symbol: " ++ unscan t)

-- Parse a boolean expression.

-- bexpr ::= bexpr and bterm | bexpr or bterm | bterm
-- bterm ::= not bterm | true | false | expr = expr | expr <= expr

-- Even though conjunction and disjunction are associative
-- and equiv precedence we follow the same process as above:

-- bexpr ::= bterm bexprOpt
-- bexprOpt ::= and bterm bexprOpt | or bterm bexprOpt | <empty>
-- bterm ::= not bterm | true | false | expr relSection
-- relSection ::= = expr | <= expr

-- Director symbols:

-- first(bexpr) = first(bterm)

-- first(bexprOpt) = {AND, OR}
-- follow(bexprOpt) = follow(bexpr) = {AND, OR, ...}

-- first(bterm) = first(expr) U {NOT, TRUE,FALSE}

-- first(relSection) = {EQUAL, LEQ}

bexpr :: [Token] -> (Bexp, [Token])

bexpr = bexprOpt . bterm

bexprOpt :: (Bexp, [Token]) -> (Bexp, [Token])

bexprOpt (bt1, AND:toks) = bexprOpt (bt1 `And` bt2, toks')
    where
    (bt2, toks') = bterm toks
bexprOpt (bt1, OR:toks)  = bexprOpt (bt1 `Or` bt2, toks')
    where
    (bt2, toks') = bterm toks
bexprOpt (bt, toks) = (bt,toks)

bterm :: [Token] -> (Bexp, [Token])

bterm (NOT:toks) = (Not bt, toks')
    where
    (bt, toks') = bterm toks
bterm (TRUE:toks) = (TrueLit, toks)
bterm (FALSE:toks) = (FalseLit, toks)
bterm toks = relSection (expr toks)

relSection :: (Aexp, [Token]) -> (Bexp, [Token])

relSection (a1, EQUAL:toks) = (a1 :=: a2, toks')
    where
    (a2, toks') = expr toks
relSection (a1, LEQ:toks) = (a1 :<=: a2, toks')
    where
    (a2, toks') = expr toks
relSection _ = error "Relational operator expected."

-- Parse commands.

-- For maximum convenience, allow "empty commands".
-- For example, {}, {c;}, {;c;;} etc.

-- I've eliminated skip from the source language, but it may still arise in
-- the abstract syntax.

-- com ::= x:=e | if bexpr then com else com | while bexpr do com | { comSeq }
-- comSeq ::= com | comSeq ; comSeq | <empty>

-- This production for comSeq reflects the associativity of `;' but leads
-- to an ambiguous grammar.  We need to solve that problem first, so we
-- may as well go for right-associativity to avoid left-recursion
-- problems:

-- comSeq ::= com | <empty> | com ; comSeq | ; comSeq

-- Now left-factoring gives:

-- comSeq ::= com seqOpt | ; comSeq | <empty>
-- seqOpt ::= ; comSeq | <empty>

-- Director symbols:

-- first(com) = {ID, IF, WHILE, LBRACE}

-- first(comSeq) = first(com) U {SEMICOLON}
-- follow(comSeq) = {RBRACE}

-- first(seqOpt) = {SEMICOLON}
-- follow(seqOpt) = {RBRACE}

-- However, since whole programs are defined by the production:

-- program ::= comSeq

-- the follower sets of comSeq and seqOpt also include end of input:

-- follow(comSeq) = {RBRACE, EOI}
-- follow(seqOpt) = {RBRACE, EOI}

-- The different followers will be accounted for in the context
-- of the com and prog parsers.

com :: [Token] -> (Com, [Token])

com (ID x:toks)  = (x := a, toks'')
    where
    toks'        = check ASSIGN toks
    (a, toks'')  = expr toks'
com (IF:toks)    = (If b c1 c2, toks2)
    where
    (b,toks')    = bexpr toks
    toks''       = check THEN toks'
    (c1,toks1)   = com toks''
    toks1'       = check ELSE toks1
    (c2,toks2)   = com toks1'
com (WHILE:toks) = (While b c, toks''')
    where
    (b, toks')   = bexpr toks
    toks''       = check DO toks'
    (c, toks''') = com toks''
com (LBRACE:toks) = (c, toks'')
    where
    (c, toks')   = comSeq toks
    toks''       = check RBRACE toks'

comSeq :: [Token] -> (Com, [Token])

comSeq []               = (Skip, [])
comSeq toks@(RBRACE:_)  = (Skip, toks)
comSeq (SEMICOLON:toks) = comSeq toks
comSeq toks             = seqOpt (com toks)

seqOpt :: (Com, [Token]) -> (Com,[Token])

seqOpt (c, [])              = (c, [])
seqOpt (c, toks@(RBRACE:_)) = (c, toks)
seqOpt (c, SEMICOLON:toks)  = seqOpt (c :~: c', toks')
    where
    (c', toks') = comSeq toks

-- Parse a whole program -- no remaining tokens

-- program ::= comSeq

parseProg :: String -> Com
parseProg source = parseProg1 $ scan source

parseProg1 :: [Token] -> Com
parseProg1 tokens = let (body, []) = comSeq tokens in body

parse :: [Token] -> Com
parse = parseProg1
