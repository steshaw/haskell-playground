--
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
      putStrLn (show ((runParser parseP5) ParseInfo {parseStream = s, parsePosition = 0}))
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
type ParseStream = L.ByteString
data ParseInfo = ParseInfo { parseStream :: ParseStream, parsePosition :: Position } deriving (Eq, Show)
type ErrorMessage = String
type ParseValue a = (a, ParseInfo)
type ParseResult a = Either ErrorMessage (ParseValue a)
type ParseFunction a = ParseInfo -> ParseResult a
data Parser a = Parser (ParseFunction a)
type ParserIgnored = Parser ()

-- Is this function really "runParser" or "getParserFunction". Would the type sig be the same for either?
runParser (Parser f) s = f s

parserBind :: Parser a -> (a -> Parser b) -> Parser b
p1 `parserBind` aToP2 = Parser $ \ s ->
  case runParser p1 s of
    Left errMsg -> Left errMsg
    Right (firstResult, firstInfo) -> runParser (aToP2 firstResult) firstInfo

instance Monad Parser where
  (>>=) = parserBind
  return a = Parser (\ s -> parseOk a s)

-- Adapted from RWH p243 (there called getState).
getInfo :: Parser ParseInfo
getInfo = Parser $ \ s -> parseOk s s

putInfo :: ParserIgnored
putInfo = Parser $ \ s -> parseOkIgnored s

parseError :: String -> ParseResult a
parseError errMsg = Left errMsg

parseOk :: a -> ParseFunction a
parseOk a = \ s -> Right (a, s)

-- Parse ok but result will be ignored or not relevant.
parseOkIgnored :: ParseFunction ()
parseOkIgnored = parseOk ()

fromMaybe :: ErrorMessage -> (ParseStream -> Maybe (a, ParseStream)) -> Parser a
fromMaybe errMsg f = Parser $ \s -> case f (parseStream s) of
  Nothing -> parseError errMsg
  Just (a, rest) -> parseOk a (ParseInfo rest 0) -- FIXME: 0 position

maybeToMaybeUnit :: Maybe L.ByteString -> Maybe ((), L.ByteString)
maybeToMaybeUnit Nothing  = Nothing
maybeToMaybeUnit (Just a) = Just ((), a)

fromMaybeUnit :: ErrorMessage -> (ParseStream -> Maybe L.ByteString) -> ParserIgnored
fromMaybeUnit errMsg f = fromMaybe errMsg (maybeToMaybeUnit . f)

checkMaxGrey :: Int -> Parser Int
checkMaxGrey grey = Parser $ \ s ->
  if grey > 0 && grey <= 255
  then parseOk grey s
  else parseError ("Illegal maxGrey value: " ++ show grey)

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: Parser Greymap
parseP5 =
  parseHeader >> skipSpaces >> parseNat >>= \ width ->
    skipSpaces >> parseNat >>= \ height ->
      skipSpaces >> parseNat >>= \ grey ->
        checkMaxGrey grey >>= \ maxGrey ->
          parseNumBytes 1 >>
            parseNumBytes (width * height) >>= \ bitmap ->
              return (Greymap (PgmInfo width height maxGrey) bitmap)

headerErrMsg = "Invalid header. Must be \"P5\"."

parseHeader :: ParserIgnored
parseHeader = Parser $ \ s ->
  if L8.pack "P5" `L8.isPrefixOf` (parseStream s)
  then parseOkIgnored s {parseStream = L.drop 2 (parseStream s), parsePosition = 2 + parsePosition s}
  else parseError headerErrMsg

parseNat :: Parser Int
parseNat =
  (fromMaybe "Cannot parse int" L8.readInt) >>= \ n ->
    getInfo >>= \ s -> (Parser $ \ s ->
      if n <= 0
      then parseError $ "Natural number must be > 0: " ++ show n
      else parseOk n s)

parseNumBytes :: Int -> Parser L.ByteString
parseNumBytes count = Parser $ \ s ->
  case L.splitAt (fromIntegral count) (parseStream s) of
    (r, _) | L.length (r) < (fromIntegral count) ->
      parseError $ "Insufficient bytes trying to get " ++ (show count) ++ " bytes"
    (r, rest) -> parseOk r (ParseInfo rest 0) -- FIXME 0 position

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

testString s = runParser parseP5 (ParseInfo (L8.pack s) 0)

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
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000"), ParseInfo L8.empty 0)
  ,test (testString "P5 1 1 255") $ Left "Insufficient bytes trying to get 1 bytes"
  ,test (testString "P5 2 2 255\n") $ Left "Insufficient bytes trying to get 4 bytes"
  ,test (testString "P5 1 1 255\n\000!") $
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000")
          ,ParseInfo (L8.pack "!") 0)
  ,test (testString $ "P5 1 1 255\n" ++ replicate (1) '\000') $
    Right (Greymap (PgmInfo {greyWidth = 1, greyHeight = 1, greyMax = 255}) (L8.pack "\000"), ParseInfo L8.empty 0)
  ,test (testString $ "P5 1 1 256\n" ++ replicate (1) '\000') $
    Left "Illegal maxGrey value: 256"
  ,test (testString $ "P5 -1" ++ replicate (1) '\000') $
    Left "Natural number must be > 0: -1"
  ]

tests = sequence_ testCases >> putStrLn ""
