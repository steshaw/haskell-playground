#!/usr/bin/env stack
{-
  stack --resolver lts-8.12 script
    --package attoparsec
    --package bytestring
    --package containers
    --
    -Wall -fwarn-tabs
-}
--
-- Adapted from https://stackoverflow.com/q/3586069/482382
--
{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad

import Data.Monoid ((<>))

import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

type Environment = M.Map B.ByteString B.ByteString

spaces :: A.Parser ()
spaces = A.skipWhile (== ' ')

lexeme :: A.Parser a -> A.Parser a
lexeme p = p <* spaces

upTo :: Char -> A.Parser B.ByteString
upTo delimiter =
  spaces *> lexeme (A.takeWhile (A.notInClass $ delimiter : " ")) <*
  (lexeme $ A.char delimiter)

entry :: A.Parser (B.ByteString, B.ByteString)
entry = (,) <$> upTo ':' <*> upTo ';'

environment :: A.Parser Environment
environment = entries <* lexeme (optional (A.char ';'))
  where
    entries = M.fromList <$> entry `A.sepBy'` A.endOfLine

top :: A.Parser a -> A.Parser a
top p = spaces *> p <* A.endOfInput

parseEnvironment :: B.ByteString -> Maybe Environment
parseEnvironment =
  A.maybeResult . flip A.feed B.empty . A.parse (top environment)

go :: B.ByteString -> IO ()
go s = do
  B.putStrLn $ "s = " <> s
  print $ A.parse (top environment) s `A.feed` ""
  print $ parseEnvironment s
  putStrLn ""

main :: IO ()
main = do
  go "  one : 1;   \n two  : 2;    "
  go " one : 1;\n two : 2;  "
  go " one : 1;\n two : 2;"
  when False $ do
    s <- B.getContents
    go s
