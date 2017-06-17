{-# LANGUAGE OverloadedStrings #-}

import Parser

import Data.Attoparsec.Text.Lazy (Result(..))
import Prelude hiding (FilePath)

import qualified Data.Attoparsec.Text.Lazy
import qualified Data.Text.Lazy.IO
import qualified Options.Generic
import qualified Prelude

process :: Prelude.FilePath -> IO ()
process fileName = do
  text <- Data.Text.Lazy.IO.readFile fileName
  case Data.Attoparsec.Text.Lazy.parse parseDerivation text of
    Fail _ _ msg -> fail ("Parse failed: " ++ msg)
    Done _ derivation -> do
      let printOutput output = print (path output)
      mapM_ printOutput (outputs derivation)

main :: IO ()
main = do
  paths <- Options.Generic.getRecord "Get the outputs of a Nix derivation"
  mapM_ process (paths :: [Prelude.FilePath])
