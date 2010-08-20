module Main where

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

checkMaxGrey r@(maxGrey, s) =
  if maxGrey > 255 || maxGrey <= 0
  then Nothing
  else Just r

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  munchString (L8.pack "P5") s >>=
    skipSpaces >>= parseNat >>= \ (width, s) ->
      skipSpaces s >>= parseNat >>= \ (height, s) ->
        skipSpaces s >>= parseNat >>= checkMaxGrey >>= \ (maxGrey, s) ->
            parseNumBytes 1 s >>= \ (_, s) ->
              parseNumBytes (width * height) s >>= \ (bitmap, s) ->
                Just (Greymap (PgmInfo width height maxGrey) bitmap, s)

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
  if L.take 1 s  == L8.pack "#"
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

parseNat :: L.ByteString -> Maybe (Int, L.ByteString)
parseNat s =
  L8.readInt s >>= \ (n, rest) ->
    if n <= 0 then Nothing else Just (n, rest)

parseNumBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
parseNumBytes count s =
  let
    result = L.splitAt (fromIntegral count) s
  in
    if L.length (fst result) < (fromIntegral count) then Nothing
    else Just result

