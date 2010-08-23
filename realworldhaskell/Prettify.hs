module Prettify where

data Doc 
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | LongOrShort Doc Doc
  deriving (Eq, Show)

empty = Empty

char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

string :: String -> Doc
string s = (Char '"') <> text s <> (Char '"')

double :: Double -> Doc
double d = text (show d)

line = Line

(<>) :: Doc -> Doc -> Doc
-- TODO: handle special case of Empty for x and y
x <> y = x `Concat` y

-- Concat for Docs.
hcat :: [Doc] -> Doc
hcat [] = Empty
hcat (x:xs) = Concat x (hcat xs)

-- Join docs together using "soft" linebreaks.
fsep :: [Doc] -> Doc
fsep = undefined

compact :: Doc -> String
compact Empty = ""
compact (Char char) = [char]
compact (Text string) = string
compact Line = "\n"
compact (Concat doc1 doc2) = (compact doc1) ++ (compact doc2)
compact (LongOrShort longDoc shortDoc) = compact shortDoc
