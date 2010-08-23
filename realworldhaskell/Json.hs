module Json where

import Steshaw
import Prettify
import Data.List (intersperse)

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
   char '{' <> (hcat $ intersperse (text "," <> line) $ map text $ map values o) <> char '}'
 where
   values (name, value) = (show name) ++ ": " ++ (renderJson value)
prettyJson  (JsonArray a) = char '[' <> (hcat $ map text $ intersperse ", " $ map renderJson a) <> char ']'
