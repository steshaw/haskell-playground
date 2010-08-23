----------------------------------------------------------------------------------------------------
module Prettify where

data Doc
  = Empty
  | Char Char
  | Text String
  | Line
  | Concat Doc Doc
  | Union Doc Doc
  deriving (Eq, Show)

empty = Empty

char c = Char c

text :: String -> Doc
text "" = Empty
text s = Text s

double :: Double -> Doc
double d = text $ show d

line = Line

(<>) :: Doc -> Doc -> Doc
-- TODO: handle special case of Empty for x and y
x <> y = x `Concat` y

fold :: (Doc -> Doc -> Doc) -> [Doc] -> Doc
fold f = foldr f empty

-- Concat for Docs.
hcat :: [Doc] -> Doc
hcat = fold Concat

fsep :: [Doc] -> Doc
fsep = fold (</>)

(</>) :: Doc -> Doc -> Doc
x </> y = x <> softline <> y

softline :: Doc
--softline = group line
-- Or just do this:
softline = Char ' ' `Union` line

-- XXX: Seems unmotivated. i.e. we don't need this
group :: Doc -> Doc
group x = flatten x `Union` x

-- XXX: Seems unmotivated. i.e. we don't need this
flatten :: Doc -> Doc
flatten (x `Concat` y) = flatten x <> flatten y
flatten Line = Char ' '
flatten (long `Union` _) = flatten long
flatten other = other

-- Join docs together using "soft" linebreaks.
fsep2 :: [Doc] -> Doc
fsep2 [] = Empty
fsep2 (x:xs) =
  let rest = fsep xs
  in Union (x <> line <> rest) (x <> char ' ' <> rest)

compact :: Doc -> String
compact Empty = ""
compact (Char char) = [char]
compact (Text string) = string
compact Line = "\n"
compact (Concat doc1 doc2) = compact doc1 ++ compact doc2
compact (Union longDoc shortDoc) = compact shortDoc

pretty width doc = best 0 [doc]
  where
    best _ [] = ""
    best col (d:ds) =
      case d of
        Empty  -> best col ds
        Char c -> c : best (col + 1) ds
        Text s -> s ++ best (col + length s) ds
        Line   -> '\n' : best 0 ds
        Concat d1 d2 -> best col (d1:d2:ds)
        Union l r    -> nicest col (best col (l:ds))
                                   (best col (r:ds))
    nicest col a b =
      if (width - least) `fits` a then a else b
      where
        least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs

prettyCrappy :: Integer -> Doc -> String
prettyCrappy width doc = go 0 doc
  where
    go col Empty = ""
    go col (Char char) = [char]
    go col (Text string) = string
    go col Line = "\n"
    go col (Concat doc1 doc2) = (go col doc1) ++ (go col doc2)
    go col (Union long short) = go col long
----------------------------------------------------------------------------------------------------
