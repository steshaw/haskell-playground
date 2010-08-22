--module Main where

import Steshaw ((>$>))

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import System.Environment (getProgName, getArgs)
import Data.List (genericDrop)
import System.IO (withFile, hPutStrLn, stderr, IOMode(ReadMode))

import Control.Monad.Error

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [file] -> withFile file ReadMode $ \ handle -> do
      s <- L.hGetContents handle
      putStrLn (show (parseP5 s))
    otherwise -> hPutStrLn stderr $ "usage: " ++ progName ++ " <file>"

data PgmInfo = PgmInfo {
  greyWidth :: Int,
  greyHeight :: Int,
  greyMax :: Int
} deriving (Eq, Show)

data Greymap = Greymap {
  pgmInfo :: PgmInfo,
  greyData :: L.ByteString
} deriving (Eq)

instance Show Greymap where
  show (Greymap info _) = show info

-- TODO: Position/ParseInfo as yet unused
type Position = Integer
data ParseInfo = ParseInfo L.ByteString Position

--type ParseResult = Either String L.ByteString
type ParseResult q a = q (a, L.ByteString)
--type ParseResult' a = ParseResult Maybe a
type ParseResult' a = ParseResult (Either String) a

parseError errMsg = Left errMsg
parseOk a rest = Right (a, rest)

fromMaybe :: (a -> Maybe L.ByteString) -> (a -> ParseResult' ())
fromMaybe f a = case f a of
  Nothing -> parseError "oops"
  Just a -> parseOk () a

fromMaybe2 :: (a -> Maybe (b, L.ByteString)) -> (a -> ParseResult' b)
fromMaybe2 f a = case f a of
  Nothing -> parseError "oops"
  Just (b, rest) -> parseOk b rest

checkMaxGrey (maxGrey, s) =
  if maxGrey > 0 && maxGrey <= 255
  then parseOk maxGrey s
  else parseError ("Illegal maxGrey value: " ++ show maxGrey)

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: L.ByteString -> ParseResult' Greymap
parseP5 s =
  fromMaybe (munchString (L8.pack "P5")) s >>= \ (_, s) ->
    fromMaybe skipSpaces s >>= \ (_, s) -> parseNat s >>= \ (width, s) ->
      fromMaybe skipSpaces s >>= \ (_, s) -> parseNat s >>= \ (height, s) ->
        fromMaybe skipSpaces s >>= \ (_, s) -> 
          parseNat s >>= checkMaxGrey >>= \ (maxGrey, s) ->
            parseNumBytes 1 s >>= \ (_, s) ->
              parseNumBytes (width * height) s >>= \ (bitmap, s) ->
                parseOk (Greymap (PgmInfo width height maxGrey) bitmap) s

parseNat :: L.ByteString -> ParseResult' Int
parseNat s =
  fromMaybe2 L8.readInt s >>= \ (n, rest) ->
    if n <= 0 
    then parseError $ "Natural number must be > 0: " ++ show n
    else parseOk n rest

parseNumBytes :: Int -> L.ByteString -> ParseResult' L.ByteString
parseNumBytes count s = (fromMaybe2 (parseNumBytesOld count)) s

parseNumBytesOld :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
parseNumBytesOld count s =
  let
    result = L.splitAt (fromIntegral count) s
  in
    if L.length (fst result) < (fromIntegral count) then Nothing
    else Just result

munchString :: L.ByteString -> L.ByteString -> Maybe L.ByteString
munchString prefix s =
  if prefix `L8.isPrefixOf` s
  then Just (L.drop (L.length prefix) s)
  else Nothing

skipSpaces :: L.ByteString -> Maybe L.ByteString
skipSpaces s =
  munchSpace s >>= \ s -> Just $ dropSpacesAndComments s

dropSpacesAndComments :: L.ByteString -> L.ByteString
dropSpacesAndComments s =
  let c = (L8.index s 0)
  in if c == '#' then dropSpacesAndComments (dropComments s)
     else if isSpace c then dropSpacesAndComments (dropSpaces s) else s

dropComments :: L.ByteString -> L.ByteString
dropComments s =
  if L8.head s  == '#'
  then L.drop 1 s >$> L8.dropWhile (/= '\n') >$> L.drop 1
  else s

dropSpaces :: L.ByteString -> L.ByteString
dropSpaces s = L8.dropWhile isSpace s

-- Must have a space.
munchSpace :: L.ByteString -> Maybe L.ByteString
munchSpace s =
  if L.null s then Nothing
  else
    if isSpace (s `L8.index` 0)
    then Just (L.drop 1 s)
    else Nothing

