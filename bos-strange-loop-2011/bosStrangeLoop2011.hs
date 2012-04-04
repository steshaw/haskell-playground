--
-- See http://bos.github.com/strange-loop-2011/
--

import Data.Char (isUpper, isLower, toLower)

import Prelude hiding (filter)
import Control.Arrow ((>>>))
import Network.HTTP.Enumerator (simpleHttp)
import Data.ByteString.Lazy.UTF8 (toString)
import Text.HTML.TagSoup
import Network.URI (parseURI, parseURIReference, uriToString, nonStrictRelativeTo)
import Data.Maybe (catMaybes)
import qualified Data.Map as Map
import qualified Data.Set as Set

data Foo = One | Two | Three
  deriving (Show, Enum)

myLength :: Num n => [a] -> n
myLength [] = 0
myLength (_:xs) = 1 + (myLength xs)

countCaps :: [Char] -> Integer
countCaps [] = 0
countCaps (c:cs) = (if isUpper c then 1 else 0) + countCaps cs

bosCountCaps :: [Char] -> Integer
bosCountCaps []    = 0
bosCountCaps (x:xs)
   | isUpper x      = 1 + bosCountCaps xs
   | otherwise      = bosCountCaps xs

caps :: [Char] -> [Char]
caps []     = []
caps (c:cs) = if isUpper c
              then c : caps cs
              else caps cs

countCaps2 :: [Char] -> Integer
countCaps2 xs = myLength $ caps xs

filter :: (a -> Bool) -> [a] -> [a]
filter _ []    = []
filter p (x:xs)
  | p x        = x : filter p xs
  | otherwise  =     filter p xs

countLowerCase :: (Num n) => [Char] -> n
countLowerCase = filter isLower >>> myLength

foo :: Integer
foo = let x = 2
          y = 4
      in x + y

-- Remove words from a sentence that do not start with a vowel.
disemvowel :: String -> String
disemvowel = unwords . (filter startsWithVowel) . words
  where
    startsWithVowel [] = False
    startsWithVowel (c:_) = (toLower c) `elem` vowels
    vowels = "aeiuo"

bosDisEmvowel =
  let isVowel c = toLower c `elem` "aeiou"
  in  unwords . filter (isVowel . head) . words

--
-- Web Spider
--

download :: String -> IO String
download url = do
  contents <- simpleHttp url
  return $ toString contents

saveAs :: String -> Int -> IO ()
saveAs contents k = writeFile (makeFileName k) contents

makeFileName :: Int -> FilePath
makeFileName k = "download-" ++ show k ++ ".html"

anchors = filter (isTagOpenName "a")

nofollow tagOpen = fromAttrib "rel" tagOpen == "nofollow"

canonicalizeLink :: String -> String -> Maybe String
canonicalizeLink referer path = do
  r <- parseURI referer
  p <- parseURIReference path
  u <- nonStrictRelativeTo p r
  return $ uriToString id u ""

links url =
  filter (\url -> (take 4 url) == "http") . -- only http urls
  catMaybes .
  map (canonicalizeLink url) .
  filter (not . null) .        -- ignore null/empty href attributes
  map (fromAttrib "href") .
  filter (not . nofollow) .
  anchors .
  canonicalizeTags .
  parseTags

processPage url = do
  page <- download url
  return (links url page)

data Link = Link String [String]
  deriving (Show)

linkFrom (Link url _) = url
linkTo (Link _ links) = links

type URL = String

spider :: Int -> URL -> IO (Map.Map URL [URL])
spider count url0 = go 0 Map.empty (Set.singleton url0)
  where
    go k seen queue0
        | k >= count = return seen
        | otherwise  =
      case Set.minView queue0 of
        Nothing -> return seen
        Just (url, queue) -> do
          page <- download url
          let ls       = links url page
              newSeen  = Map.insert url ls seen
              notSeen  = Set.fromList .
                         filter (`Map.notMember` newSeen) $ ls
              newQueue = queue `Set.union` notSeen
          go (k+1) newSeen newQueue
