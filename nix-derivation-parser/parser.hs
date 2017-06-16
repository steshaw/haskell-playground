{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

import Filesystem.Path.CurrentOS (FilePath)
import Data.Map (Map)
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Prelude hiding (FilePath)
import Control.Monad (void)

import Data.Attoparsec.Text.Lazy (Parser)

import qualified Data.Map
import qualified Data.Set
import qualified Data.Vector
import qualified Data.Attoparsec.Text.Lazy

data Derivation = Derivation
  { outputs   :: Map Text DerivationOutput  -- ^ keyed on symbolic IDs
  , inputDrvs :: Map FilePath (Set Text)
  , inputSrcs :: Set FilePath               -- ^ inputs that are sources
  , platform  :: Text
  , builder   :: Text
  , args      :: Vector Text
  , env       :: Map Text Text
  } deriving (Show)

data DerivationOutput = DerivationOutput
    { path     :: FilePath
    , hashAlgo :: Text    -- ^ hash used for expected hash computation
    , hash     :: Text    -- ^ expected hash, may be null
    } deriving (Show)

todo = error "TODO"

-- | Given a parser for an element, return a parser for a list of elements.
listOf :: Parser a -> Parser [a]
listOf element = do
  void "["
  es <- Data.Attoparsec.Text.Lazy.sepBy element ","
  void "]"
  pure es

vectorOf :: Parser a -> Parser (Vector a)
vectorOf element = do
  es <- listOf element
  pure $ Data.Vector.fromList es

setOf :: Ord a => Parser a -> Parser (Set a)
setOf element = do
  es <- listOf element
  pure $ Data.Set.fromList es

mapOf :: Ord k => Parser (k, v) -> Parser (Map k v)
mapOf keyValue = do
  keyValues <- listOf keyValue
  pure $ Data.Map.fromList keyValues

string :: Parser Text
string = todo

filepath :: Parser FilePath
filepath = todo

parseDerivation :: Parser Derivation
parseDerivation = do
    void "Derive("

    let keyValue0 :: Parser (Text, DerivationOutput)
        keyValue0 = do
            void "("
            key <- string
            void ","
            path <- filepath
            void ","
            hashAlgo <- string
            void ","
            hash <- string
            void ")"
            return (key, DerivationOutput {..})

    outputs <- mapOf keyValue0

    void ","

    let keyValue1 = do
         void "("
         key <- filepath
         void ","
         value <- setOf string
         void ")"
         return (key, value)
    inputDrvs <- mapOf keyValue1

    void ","

    inputSrcs <- setOf filepath
    void ","
    platform <- string
    void ","
    builder <- string
    void ","
    args <- vectorOf string
    void ","

    let keyValue2 = do
          void "("
          key <- string
          void ","
          value <- string
          void ")"
          return (key, value)
    env <- mapOf keyValue2

    void ")"

    todo

main :: IO ()
main = putStrLn "nix-drv"
