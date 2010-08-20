
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as L

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

data Greymap = Greymap {
  greyWidth :: Int,
  greyHeight :: Int,
  greyMax :: Int,
  greyData :: L.ByteString
} deriving (Eq, Show)

parseP5 :: L.ByteString -> Maybe (Greymap, L.ByteString)

-- Parse: <P5> <width> <height> <maxGrey> <binaryImageData>
-- FIXME: parse whitespace
parseP5 s =
  case skipHeader (L8.pack "P5") s of
    Nothing -> Nothing
    Just s ->
      case parseNat s of
        Nothing -> Nothing
        Just (width, s) ->
          case parseNat s of
            Nothing -> Nothing
            Just (height, s) ->
              case parseNat s of
                Nothing -> Nothing
                Just (maxGrey, s) ->
                  case parseNumBytes (width * height) s of
                    Nothing -> Nothing
                    Just (bitmap, s) -> Just (Greymap width height maxGrey bitmap, s)

skipHeader :: L.ByteString -> L.ByteString -> Maybe L.ByteString
skipHeader prefix s = 
  if prefix `L8.isPrefixOf` s
  then Just (L.drop (L.length prefix) s)
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
