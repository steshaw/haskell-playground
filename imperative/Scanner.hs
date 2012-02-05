-- Scanner for Imp.
-- Developed for use in COMP3610
-- Clem Baker-Finch

module Scanner (Token(..), scan, unscan) where

import Data.Char

data Token = ID String
           | NUM Int
           | PLUS | MINUS | TIMES
           | TRUE | FALSE | EQUAL | LEQ
           | NOT | AND | OR
           | ASSIGN | LBRACE | RBRACE
           | IF | THEN | ELSE | WHILE | DO
           | SEMICOLON | LPAREN | RPAREN deriving (Eq, Show)

-- The scan function returns a list of tokens.

scan :: String -> [Token]

scan []           = []
scan ('{':'-':cs) = scan (skipComment cs 1)
scan ('='    :cs) = EQUAL     : scan cs
scan ('+'    :cs) = PLUS      : scan cs
scan ('-'    :cs) = MINUS     : scan cs
scan ('*'    :cs) = TIMES     : scan cs
scan (';'    :cs) = SEMICOLON : scan cs
scan ('{'    :cs) = LBRACE    : scan cs
scan ('}'    :cs) = RBRACE    : scan cs
scan ('('    :cs) = LPAREN    : scan cs
scan (')'    :cs) = RPAREN    : scan cs
scan ('<':'=':cs) = LEQ       : scan cs
scan (':':'=':cs) = ASSIGN    : scan cs
scan input@(c:cs)
    | isSpace c   = scan cs
    | isAlpha c   = let (word, afterWord) = span isAlphaNum input
                    in checkResWord word : scan afterWord
    | isDigit c   = let (num,  afterNum)  = span isDigit input
                    in NUM (read num)    : scan afterNum
    | otherwise   = error (c:" : illegal character.")

-- Skip comments delimited by {- ... -}.
-- Handles nested comments. The 2nd argument is the nesting depth.

skipComment :: String -> Int -> String

skipComment []  depth    = error "End of input while scanning comment."
skipComment [_] depth    = error "End of input while scanning comment."
skipComment ('-':'}':cs) depth
    | depth == 1         = cs
    | otherwise          = skipComment cs (depth-1)
skipComment ('{':'-':cs) depth
                         = skipComment cs (depth+1)
skipComment (_:cs) depth = skipComment cs depth

-- Distinguish reserved words from identifiers.
-- Case sensitive for simplicity.

checkResWord :: String -> Token

checkResWord "true"  = TRUE
checkResWord "false" = FALSE
checkResWord "not"   = NOT
checkResWord "and"   = AND
checkResWord "or"    = OR
checkResWord "if"    = IF
checkResWord "then"  = THEN
checkResWord "else"  = ELSE
checkResWord "while" = WHILE
checkResWord "do"    = DO
checkResWord other   = ID other

-- For better error messages, convert tokens back to strings.
-- Some redundancy, no doubt.

unscan :: Token -> String

unscan (ID x)    = "identifier " ++ x
unscan (NUM n)   = "number " ++ show n
unscan PLUS      = "+"
unscan MINUS     = "-"
unscan TIMES     = "*"
unscan TRUE      = "true"
unscan FALSE     = "false"
unscan EQUAL     = "="
unscan LEQ       = "<="
unscan NOT       = "not"
unscan AND       = "and"
unscan OR        = "or"
unscan ASSIGN    = ":="
unscan LBRACE    = "{"
unscan RBRACE    = "}"
unscan IF        = "if"
unscan THEN      = "then"
unscan ELSE      = "else"
unscan WHILE     = "while"
unscan DO        = "do"
unscan SEMICOLON = ";"
unscan LPAREN    = "("
unscan RPAREN    = ")"
