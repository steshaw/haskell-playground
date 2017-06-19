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

import Control.Monad

import Data.Monoid ((<>))

import qualified Control.Applicative as A (many)
import qualified Data.Attoparsec.ByteString.Char8 as A
import qualified Data.ByteString.Char8 as B
import qualified Data.Map as M

type Environment = M.Map B.ByteString B.ByteString

spaces :: A.Parser String
spaces = A.many $ A.char ' '

upTo :: Char -> A.Parser B.ByteString
upTo delimiter =
  spaces *> A.takeWhile (A.notInClass $ delimiter : " ") <* spaces <*
  A.char delimiter <*
  spaces

entry :: A.Parser (B.ByteString, B.ByteString)
entry = (,) <$> upTo ':' <*> upTo ';'

environment :: A.Parser Environment
environment = M.fromList <$> entry `A.sepBy'` A.endOfLine

parseEnvironment :: B.ByteString -> Maybe Environment
parseEnvironment = A.maybeResult . flip A.feed B.empty . A.parse environment

go :: B.ByteString -> IO ()
go s = do
  B.putStrLn $ "s = " <> s
  print $ A.parse environment s `A.feed` ""
  print $ parseEnvironment s
  putStrLn ""

main :: IO ()
main = do
  go "  one : 1; two  : 2;    "
  go " one : 1;\n two : 2;"
  when False $ do
    s <- B.getContents
    go s
