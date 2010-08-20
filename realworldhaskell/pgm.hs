
import Steshaw

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L
import Data.Char (isSpace, chr)

import System.Environment (getProgName, getArgs)
import Data.List (genericDrop)
import System.IO (withFile, hPutStrLn, stderr, IOMode(ReadMode))

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


onJust f x =
  case x of
    Nothing -> Nothing
    Just a -> f a

{-
newParseP5 =
--  skipHeader (L8.pack "P5") >$> onJust (\ s -> Just (L8.pack "FIXME"))
  onJust (\ s -> Just (L8.pack "FIXME")) (skipHeader (L8.pack "P5"))
-}

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)
parseP5 s =
  case skipHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s ->
      case skipSpaces s of
        Nothing -> Nothing
        Just s  ->
          case parseNat s of
            Nothing -> Nothing
            Just (width, s) ->
              case skipSpaces s of
                Nothing -> Nothing
                Just s  ->
                  case parseNat s of
                    Nothing -> Nothing
                    Just (height, s) ->
                      case skipSpaces s of
                        Nothing -> Nothing
                        Just s  ->
                          case parseNat s of
                            Nothing -> Nothing
                            Just (maxGrey, s) ->
                              if maxGrey > 255 || maxGrey <= 0 then Nothing
                              else
                                case skipSpaces s of
                                  Nothing -> Nothing
                                  Just s  -> 
                                    case parseNumBytes (width * height) s of
                                      Nothing -> Nothing
                                      Just (bitmap, s) -> Just (Greymap (PgmInfo width height maxGrey) bitmap, s)

skipHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
skipHeader prefix s =
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
  let c = (L.index s 0) >$> word8ToChar
  in if c == '#' then dropSpacesAndComments (dropComments s)
     else if isSpace c then dropSpacesAndComments (dropSpaces s) else s

dropComments :: L.ByteString -> L.ByteString
dropComments s =
  if (L.take 1 s  == L8.pack "#") 
  then L.drop 1 s >$> L.dropWhile (word8ToChar >.> (/= '\n')) >$> L.drop 1
  else s

dropSpaces :: L.ByteString -> L.ByteString
dropSpaces s = L.dropWhile (word8ToChar >.> isSpace) s

-- Must have a space. Munch it.
takeSpace :: L.ByteString -> Maybe L.ByteString
takeSpace s =
  if L.null s then Nothing
  else
    if isSpace (word8ToChar $ s `L.index` 0)
    then Just (L.drop 1 s)
    else Nothing

word8ToChar w8 = chr $ fromIntegral w8

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
