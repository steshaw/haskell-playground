{-# LANGUAGE OverloadedStrings #-}

import Parser

import Prelude hiding (FilePath)
import Data.Monoid ((<>))

import qualified Data.Attoparsec.ByteString.Lazy as P
import qualified Data.ByteString.Lazy.Char8 as SIO
import qualified Options.Generic
import qualified Prelude
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text.IO as TIO

process :: Prelude.FilePath -> IO ()
process fileName = do
  text <- SIO.readFile fileName
  case P.eitherResult (P.parse parseDerivation text) of
    Left msg -> fail ("Parse failed: " ++ msg)
    Right derivation -> do
      let printOutput output =
           let r = FS.toText (path output)
               t = case r of
                     Left brokenFilePath -> "Broken: " <> brokenFilePath
                     Right filePath_ -> filePath_
           in TIO.putStrLn t
      mapM_ printOutput (outputs derivation)

main :: IO ()
main = do
  paths <- Options.Generic.getRecord "Get the outputs of a Nix derivation"
  mapM_ process (paths :: [Prelude.FilePath])
