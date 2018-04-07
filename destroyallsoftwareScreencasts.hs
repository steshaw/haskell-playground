#!/usr/bin/env stack
{-
  stack
    --resolver lts-11.3 script
    --package async
    --package bytestring
    --package lens
    --package text
    --package wreq
    --package xml-conduit
-}

{-

Destroy All Software Screencast Scraper.

Since DAS are having a "sale" at the moment and all screencasts are
free to watch and download. This program is a little scraper to
find all the 1080p screencast urls.

See <https://www.destroyallsoftware.com/sale>.

Expected usage:

$ destroyallsoftwareScreencasts.hs >destroyallsoftwareScreencasts.txt
$ vim destroyallsoftwareScreencasts.txt # you better check it isn't Bollocks up.
$ wget -ci destroyallsoftwareScreencasts.txt # download screencasts

-}

{-# LANGUAGE OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent.Async
import Control.Lens hiding (element)
import Control.Monad
import Data.Function
import Data.Semigroup
import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Text.Read (decimal)
import Data.Monoid (mconcat)
import Network.Wreq

import GHC.Exts (fromList)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.IO as TIO

getScript :: Document -> [Text]
getScript document = fromDocument document $// element "script" &/ content

parseItem :: Cursor -> [Text]
parseItem cursor = cursor $/ element "link" &/ content

tshow :: Show a => a -> Text
tshow = fromList . show

main :: IO ()
main = do
  doc <- do
    r <- get "https://www.destroyallsoftware.com/screencasts/feed"
    pure $ r ^. responseBody & Text.XML.parseLBS_ def
  let cursor = fromDocument doc
  let links = cursor $// element "item" >=> parseItem
  (bodies :: [(Text, Text)]) <- (flip mapConcurrently) links $ \link -> do
    r <- get (T.unpack link)
    pure (link, r ^. responseBody & BSL.toStrict & decodeUtf8)
  forM_ bodies $ \(link, body) -> do
    let isMovie line = T.isInfixOf "source.src" line && T.isInfixOf ".mp4" line
    let movies = filter isMovie (T.lines body)
    let m1080p = case movies of
                   [m4k, m1080p] -> m1080p
                   _             -> ""
    case T.splitOn "\"" m1080p of
      [before, it, after] -> TIO.putStrLn it
      _                   -> TIO.putStrLn $ "Bollocks: " <> tshow (T.splitOn "\"" m1080p)
