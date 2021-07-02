-- From https://github.com/nikita-volkov/hasql/#example

{-# LANGUAGE OverloadedStrings #-}

import Prelude

import Data.Int ( Int64 )
import Data.Functor.Contravariant ( (>$<) )
import Hasql.Session (Session)
import Hasql.Statement (Statement(..))

import qualified Hasql.Session as S
import qualified Hasql.Decoders as D
import qualified Hasql.Encoders as E
import qualified Hasql.Connection as C

main :: IO ()
main = do
  connectionE <- C.acquire connectionSettings
  case connectionE of
    Left s -> print s
    Right connection -> do
      result <- S.run (sumAndDivModSession 3 8 3) connection
      print result
  where
    host = "localhost"
    port = 5432
    user = "postgres"
    password = "password"
    database = "postgres"
    connectionSettings = C.settings host port user password database

-- * Sessions
--
-- Session is an abstraction over the database connection and all possible
-- errors.
-- It is used to execute statements.
-- It is composable and has a Monad instance.
--
-- It's recommended to define sessions in a dedicated 'Sessions' submodule of
-- your project.
-------------------------

sumAndDivModSession
  :: Int64 -- ^
  -> Int64 -- ^
  -> Int64 -- ^
  -> Session (Int64, Int64)
sumAndDivModSession a b c = do
  -- Get the sum of a and b
  total <- S.statement (a, b) sumStatement
  -- Divide the sum by c and get the modulo as well
  S.statement (total, c) divModStatement

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
sumStatement =
  Statement sql encoder decoder True
 where
  sql = "select $1 + $2"
  encoder =
    (fst >$< E.param (E.nonNullable E.int8)) <>
    (snd >$< E.param (E.nonNullable E.int8))
  decoder = D.singleRow (D.column (D.nonNullable D.int8))

divModStatement :: Statement (Int64, Int64) (Int64, Int64)
divModStatement = Statement sql encoder decoder True
 where
  sql = "select $1 / $2, $1 % $2"
  encoder =
    (fst >$< E.param (E.nonNullable E.int8)) <>
    (snd >$< E.param (E.nonNullable E.int8))
  decoder = D.singleRow row where
    row =
      (,) <$>
      D.column (D.nonNullable D.int8) <*>
      D.column (D.nonNullable D.int8)
