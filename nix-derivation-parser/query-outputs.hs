{-# LANGUAGE OverloadedStrings #-}

import Parser

import Prelude hiding (FilePath)

import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString.Lazy.Char8 as SIO
import qualified Options.Generic
import qualified Prelude

process :: Prelude.FilePath -> IO ()
process fileName = do
  text <- SIO.readFile fileName
  case P.eitherResult (P.parse parseDerivation text) of
    Left msg -> fail ("Parse failed: " ++ msg)
    Right derivation -> do
      let printOutput output = print (path output)
      mapM_ printOutput (outputs derivation)

main :: IO ()
main = do
  paths <- Options.Generic.getRecord "Get the outputs of a Nix derivation"
  mapM_ process (paths :: [Prelude.FilePath])
