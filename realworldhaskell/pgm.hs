module Main where

import Steshaw

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr)

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

--
-- My own concoction of >>? from RWH p238 (in my attempt to refactor step-by-step instead of
-- introducing a prebaked abstraction). Basically monadic bind (>>=) for Maybe.
--
onJust f x =
  case x of
    Nothing -> Nothing
    Just a -> f a

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  munchString (L8.pack "P5") s >$> onJust $$ \ s ->
    skipSpaces s >$> onJust $$ \ s ->
      parseNat s >$> onJust $$ \ (width, s) ->
        skipSpaces s >$> onJust $$ \ s ->
          parseNat s >$> onJust $$ \ (height, s) ->
            skipSpaces s >$> onJust $$ \ s ->
              parseNat s >$> onJust $$ \ (maxGrey, s) ->
                if maxGrey > 255 || maxGrey <= 0 then Nothing
                else Just (maxGrey, s) >$> onJust $$ \_ ->
                  skipSpaces s >$> onJust $$ \s ->
                    parseNumBytes (width * height) s >$> onJust $$ \ (bitmap, s) ->
                      Just (Greymap (PgmInfo width height maxGrey) bitmap, s)

munchString :: L.ByteString -> L.ByteString -> Maybe L.ByteString
munchString prefix s =
  if prefix `L8.isPrefixOf` s
  then Just (L.drop (L.length prefix) s)
  else Nothing

skipSpaces :: L.ByteString -> Maybe L.ByteString
skipSpaces s =
  let
    a = takeSpace s
    b = case a of
          Nothing -> Nothing
          Just (s) -> Just $ dropSpacesAndComments s
  in b

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

-- Must have a space. Munch it.
takeSpace :: L.ByteString -> Maybe L.ByteString
takeSpace s =
  if L.null s then Nothing
  else
    if isSpace (s `L8.index` 0)
    then Just (L.drop 1 s)
    else Nothing

parseNat :: L.ByteString -> Maybe (Int, L.ByteString)
parseNat s =
  case L8.readInt s of
    Nothing -> Nothing
    Just (n, rest) -> if n <= 0 then Nothing else Just (n, rest)

parseNumBytes :: Int -> L.ByteString -> Maybe (L.ByteString, L.ByteString)
parseNumBytes count s =
  let
    result = L.splitAt (fromIntegral count) s
  in
    if L.length (fst result) < (fromIntegral count) then Nothing
    else Just result

