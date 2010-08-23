module Json where

import Steshaw
import Prettify
import Data.List (intersperse)
import Numeric (showHex)
import Data.Char (ord)
import Data.Bits (shiftR, (.&.), (.|.))

data Json
  = JsonString String
  | JsonNumber Double
  | JsonBool Bool
  | JsonNull
  | JsonObject [(String, Json)]
  | JsonArray [Json]
    deriving (Eq, Ord, Show)

renderJson (JsonString s) = show s
renderJson (JsonNumber n) = show n
renderJson (JsonBool b)   = if b then "true" else "false"
renderJson JsonNull = "null"
renderJson (JsonObject o) = "{" ++ (concat $ intersperse ", " $ map values o) ++ "}"
 where values (name, value) = (show name) ++ ": " ++ (renderJson value)
renderJson (JsonArray a) = "[" ++ (concat $ intersperse ", " $ map renderJson a) ++ "]"

prettyJson :: Json -> Doc
prettyJson (JsonString s) = string s
prettyJson (JsonNumber n) = double n
prettyJson (JsonBool b)   = text $ if b then "true" else "false"
prettyJson JsonNull = text "null"
prettyJson (JsonObject o) =
  char '{' <> (hcat $ intersperse (text "," </> empty) $ map text $ map values o) <> char '}'
   where
     values (name, value) = (show name) ++ ": " ++ (renderJson value)
prettyJson  (JsonArray a) = 
  char '[' <> (hcat $ intersperse (text ", " </> empty) $ map prettyJson a) <> char ']'

value = JsonObject [("pi", JsonNumber 3.14), ("q", JsonBool True)]

string :: String -> Doc
string s = Char '"' <> (hcat $ map jsonChar s) <> Char '"'

jsonChar :: Char -> Doc
jsonChar c = 
  case lookup c simpleEscapes of
    Just r -> text r
    Nothing | mustEscape c -> hexEscape c
            | otherwise    -> char c
  where
    mustEscape c = c < ' ' || c == '\x7F' || c > '\xFF'

simpleEscapes :: [(Char, String)]
simpleEscapes = zipWith ch "\b\n\f\r\t\\\"/" "bnfrt\\\"/"
  where ch a b = (a, ['\\', b])

smallHex :: Int -> Doc
smallHex x = text "\\u"
  <> text (replicate (4 - length h) 'o')
  <> text h
  where h = showHex x ""

astral :: Int -> Doc
astral n = smallHex (a + 0xD800) <> smallHex (b + 0xDC00)
  where
    a = (n `shiftR` 10) .&. 0x3FF
    b = n .&. 0x3FF

hexEscape :: Char -> Doc
hexEscape c | d < 0x10000 = smallHex d
            | otherwise   = astral (d - 0x10000)
  where d = ord c
