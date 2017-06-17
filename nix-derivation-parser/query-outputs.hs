{-# LANGUAGE OverloadedStrings #-}

import Parser

import Data.Attoparsec.Text (IResult(..))
import Prelude hiding (FilePath)

import qualified Data.Attoparsec.Text
import qualified Data.Text.IO
import qualified Options.Generic
import qualified Prelude

process :: Prelude.FilePath -> IO ()
process fileName = do
  text <- Data.Text.IO.readFile fileName
  case Data.Attoparsec.Text.parse parseDerivation text of
    Fail _ _ msg -> fail ("Parse failed: " ++ msg)
    Partial _k -> fail ("Parse failed: Partial")
    Done _ derivation -> do
      let printOutput output = print (path output)
      mapM_ printOutput (outputs derivation)

main :: IO ()
main = do
  paths <- Options.Generic.getRecord "Get the outputs of a Nix derivation"
  mapM_ process (paths :: [Prelude.FilePath])
