{-# language InstanceSigs #-}

{- CIS 194 HW 10
   due Monday, 1 April
-}

module AParser where

import           Control.Applicative

import           Data.Char
import Control.Arrow (first)

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

instance Functor Parser where
  fmap f p = Parser (\s -> first f <$> runParser p s)

instance Applicative Parser where
  pure a = Parser (\s -> Just (a, s))
  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  p1 <*> p2 = Parser (\s -> case runParser p1 s of
                   Nothing -> Nothing
                   Just (f, s2) -> case runParser p2 s2 of
                                       Nothing -> Nothing
                                       Just (a, s3) -> Just (f a, s3))

newtype Name  = Name String
newtype Phone = Phone String

data Employee = Employee {name :: Name, phone :: Phone}

parseName :: Parser Name
parseName = undefined

parsePhone :: Parser Phone
parsePhone = undefined

parseEmployee :: Parser Employee
parseEmployee = Employee <$> parseName <*> parsePhone

abParser :: Parser (Char, Char)
abParser = (,) <$> char 'a' <*> char 'b'

abParser_ :: Parser ()
abParser_ = const () <$> abParser

intPair :: Parser (Integer, Integer)
intPair = f <$> posInt <*> char ' ' <*> posInt
  where
    f i1 _ i2 = (i1, i2)

{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f b -> f c
-}

instance Alternative Parser where
  empty = Parser (const Nothing)
  p1 <|> p2 = Parser (\s -> case runParser p1 s of
    Nothing -> runParser p2 s
    r       -> r)

intOrUppercase :: Parser ()
intOrUppercase = const () <$> posInt <|> const () <$> satisfy isUpper

