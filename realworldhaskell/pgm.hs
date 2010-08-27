--
-- PGM parser. See RWH Ch10.
--
module Main where

import Steshaw ((>$>))

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace)

import System.Environment (getProgName, getArgs)
import System.IO (withFile, hPutStrLn, stderr, IOMode(ReadMode))

import Control.Monad.Error () -- Just need Monad (Either e) instance. Strange that this works.
import Debug.Trace

main :: IO ()
main = do
  args <- getArgs
  progName <- getProgName
  case args of
    [file] -> withFile file ReadMode $ \ handle -> do
      s <- L.hGetContents handle
      putStrLn (show ((runParser parseP5) s))
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

type ParseStream = L.ByteString
type ErrorMessage = String
type ParseValue a = (a, ParseStream) -- value produced + rest of byte stream to parse
type ParseResult a = Either ErrorMessage (ParseValue a)
type ParseFunction a = ParseStream -> ParseResult a
data Parser a = Parser (ParseFunction a)
type ParserIgnored = Parser ()

runParser (Parser f) s = f s

-- Adapted from RWH p243 (there called getState).
getStream :: Parser ParseStream
getStream = Parser $ \ s -> parseOk s s

putStream :: ParserIgnored
putStream = Parser $ \ s -> parseOkIgnored s

parseError :: String -> ParseResult a
parseError errMsg = Left errMsg

parseOk :: a -> ParseFunction a
parseOk a s = Right (a, s)

-- Parse ok but result will be ignored or not relevant.
parseOkIgnored = \ rest -> parseOk () rest

fromMaybe :: ErrorMessage -> (ParseStream -> Maybe (a, ParseStream)) -> Parser a
fromMaybe errMsg f = Parser $ \s -> case f s of
  Nothing -> parseError errMsg
  Just (b, rest) -> parseOk b rest

maybeToMaybe :: Maybe L.ByteString -> Maybe ((), L.ByteString)
maybeToMaybe Nothing  = Nothing
maybeToMaybe (Just a) = Just ((), a)

fromMaybeUnit :: ErrorMessage -> (ParseStream -> Maybe L.ByteString) -> ParserIgnored
fromMaybeUnit errMsg f = fromMaybe errMsg (maybeToMaybe . f)

checkMaxGrey :: Int -> Parser Int
checkMaxGrey grey = Parser $ \ s ->
  if grey > 0 && grey <= 255
  then parseOk grey s
  else parseError ("Illegal maxGrey value: " ++ show grey)

parserBind :: Parser a -> (a -> Parser b) -> Parser b
p1 `parserBind` aToP2 = Parser $ \ s ->
  case runParser p1 s of
    Left errMsg -> Left errMsg
    Right (firstResult, newStream) -> runParser (aToP2 firstResult) newStream

(!>>) :: Parser a -> Parser b -> Parser b
p1 !>> p2 = p1 `parserBind` \_ -> p2

instance Monad Parser where
  (>>=) = parserBind
  return a = Parser (\ s -> parseOk a s)

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: Parser Greymap
parseP5 =
  parseHeader >> skipSpaces >> parseNat >>= \ (width) ->
    skipSpaces >>
      parseNat >>= \ height ->
        skipSpaces >> parseNat >>= \grey -> 
          checkMaxGrey grey >>= \ maxGrey ->
            parseNumBytes 1 >>
              parseNumBytes (width * height) >>= \ bitmap -> 
                return (Greymap (PgmInfo width height maxGrey) bitmap)

headerErrMsg = "Invalid header. Must be \"P5\"."

parseHeader :: ParserIgnored
parseHeader = Parser $ \s ->
  if L8.pack "P5" `L8.isPrefixOf` s
  then parseOkIgnored $ L.drop 2 s
  else parseError headerErrMsg

parseNat :: Parser Int
parseNat =
  (fromMaybe "Cannot parse int" L8.readInt) >>= \ n ->
    getStream >>= \s -> (Parser $ \ s ->
      if n <= 0
      then parseError $ "Natural number must be > 0: " ++ show n
      else parseOk n s)

parseNumBytes :: Int -> Parser L.ByteString
parseNumBytes count = Parser $ \ s ->
  case L.splitAt (fromIntegral count) s of
    (r, _) | L.length (r) < (fromIntegral count) ->
      parseError $ "Insufficient bytes trying to get " ++ (show count) ++ " bytes"
    (r, rest) -> parseOk r rest

skipSpaces :: ParserIgnored
skipSpaces = fromMaybeUnit "Cannot skip spaces" skipSpacesOld

skipSpacesOld :: L.ByteString -> Maybe L.ByteString
skipSpacesOld s =
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

--
-- Some test cases.
--

testString s = runParser parseP5 $ L8.pack s

test :: (Show a, Eq a) => a -> a -> IO ()
test actual expected = do
  if actual /= expected
    then do
      putStrLn "FAILURE:"
      putStrLn $ "actual  : " ++ (show actual)
      putStrLn $ "expected: " ++ (show expected)
      putStrLn ""
    else putStr "."

testCases =
  [
   test (testString "") $ Left headerErrMsg
  ,test (testString "1") $ Left headerErrMsg
  ,test (testString "12") $ Left headerErrMsg
  ,test (testString "p2") $ Left headerErrMsg
  ,test (testString "p5") $ Left headerErrMsg
  ,test (testString "P5") $ Left "Cannot skip spaces"
  ,test (testString "P5  foo") $ Left "Cannot parse int"
  ,test (testString "P5 1 1 255\n\000") $
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000"), L8.empty)
  ,test (testString "P5 1 1 255") $ Left "Insufficient bytes trying to get 1 bytes"
  ,test (testString "P5 2 2 255\n") $ Left "Insufficient bytes trying to get 4 bytes"
  ,test (testString "P5 1 1 255\n\000!") $
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000")
          ,L8.pack "!")
  ,test (testString $ "P5 1 1 255\n" ++ replicate (1) '\000') $
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000"), L8.empty)
  ,test (testString $ "P5 1 1 256\n" ++ replicate (1) '\000') $
    Left "Illegal maxGrey value: 256"
  ,test (testString $ "P5 -1" ++ replicate (1) '\000') $
    Left "Natural number must be > 0: -1"
  ]

tests = sequence_ testCases >> putStrLn ""
