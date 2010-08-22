--module Main where

import Steshaw ((>$>))

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import System.Environment (getProgName, getArgs)
import Data.List (genericDrop)
import System.IO (withFile, hPutStrLn, stderr, IOMode(ReadMode))

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

checkValid :: (a -> Bool) -> a -> Maybe a
checkValid f a = if f a then Just a else Nothing

checkMaxGrey parseResult =
    checkValid goodMaxGrey parseResult
  where
    goodMaxGrey (maxGrey, s) = maxGrey > 0 && maxGrey <= 255

-- TODO: Position/ParseInfo as yet unused
type Position = Integer
data ParseInfo = ParseInfo L.ByteString Position

--type ParseResult = Either String L.ByteString
type ParseResult q a = q (a, L.ByteString)
type ParseResult' a = ParseResult Maybe a

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: L.ByteString -> ParseResult' Greymap
parseP5 s =
  munchString (L8.pack "P5") s >>= skipSpaces >>= parseNat >>= \ (width, s) ->
    skipSpaces s >>= parseNat >>= \ (height, s) ->
      skipSpaces s >>= parseNat >>= checkMaxGrey >>= \ (maxGrey, s) ->
          parseNumBytes 1 s >>= \ (_, s) ->
            parseNumBytes (width * height) s >>= \ (bitmap, s) ->
              Just (Greymap (PgmInfo width height maxGrey) bitmap, s)

parseNat :: L.ByteString -> ParseResult' Int
parseNat s =
  L8.readInt s >>= \ (n, rest) ->
    if n <= 0 then Nothing else Just (n, rest)

parseNumBytes :: Int -> L.ByteString -> ParseResult' L.ByteString
parseNumBytes count s =
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

