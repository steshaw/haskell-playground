#!/usr/bin/env stack
{-
  stack script --resolver lts-12.14
    --package contravariant
    --package hasql
-}

-- From https://github.com/nikita-volkov/hasql/#example

{-# LANGUAGE OverloadedStrings #-}

import Prelude

import Data.Int
import Data.Functor.Contravariant
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))

import qualified Hasql.Session as Session
import qualified Hasql.Decoders as Decoders
import qualified Hasql.Encoders as Encoders
import qualified Hasql.Connection as Connection

main :: IO ()
main = do
  Right connection <- Connection.acquire connectionSettings
  result <- Session.run (sumAndDivModSession 3 8 3) connection
  print result
  where
    connectionSettings = Connection.settings "localhost" 5432 "postgres" "" "postgres"

-- * Sessions
--
-- Session is an abstraction over the database connection and all possible errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.
--
-- It's recommended to define sessions in a dedicated 'Sessions'
-- submodule of your project.
-------------------------

sumAndDivModSession :: Int64 -> Int64 -> Int64 -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  -- Get the sum of a and b
  sumOfAAndB <- Session.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  Session.statement (sumOfAAndB, c) divModStatement

-- * Statements
--
-- Statement is a definition of an individual SQL-statement,
-- accompanied by a specification of how to encode its parameters and
-- decode its result.
--
-- It's recommended to define statements in a dedicated 'Statements'
-- submodule of your project.
-------------------------

sumStatement :: Statement (Int64, Int64) Int64
sumStatement = Statement sql encoder decoder True where
  sql = "select $1 + $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow (Decoders.column Decoders.int8)

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder True where
  sql = "select $1 / $2, $1 % $2"
  encoder =
    (fst >$< Encoders.param Encoders.int8) <>
    (snd >$< Encoders.param Encoders.int8)
  decoder = Decoders.singleRow row where
    row =
      (,) <$>
      Decoders.column Decoders.int8 <*>
      Decoders.column Decoders.int8
