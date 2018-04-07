#!/usr/bin/env stack
{-
  stack --resolver lts-11.3 exec
    --package async
    --package bytestring
    --package lens
    --package text
    --package wreq
    --package xml-conduit
    --
    ghc --make -Wall -O2 -threaded -with-rtsopts=-N
-}

{-

Destroy All Software Screencast Scraper.

Since DAS are having a "sale" at the moment and all screencasts are
free to watch and download. This program is a little scraper to
find all the 1080p screencast urls.

See <https://www.destroyallsoftware.com/sale>.

Expected usage:

$ ./destroyallsoftwareScreencasts.hs # compiles to ./destroyallsoftwareScreencasts
$ time ./destroyallsoftwareScreencasts >destroyallsoftwareScreencasts.txt
$ vim destroyallsoftwareScreencasts.txt # you better check it isn't Bollocks up.
$ wget -ci destroyallsoftwareScreencasts.txt # download screencasts

-}

{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

import Control.Concurrent (rtsSupportsBoundThreads)
import Control.Concurrent.Async (mapConcurrently)
import Control.Lens hiding (element)
import Control.Monad
import Data.Semigroup
import Text.XML
import Text.XML.Cursor
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Network.Wreq
import System.IO (stderr)

import GHC.Conc
import GHC.Exts (fromList)

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

{-
getScript :: Document -> [Text]
getScript document = fromDocument document $// element "script" &/ content
-}

tshow :: Show a => a -> Text
tshow = fromList . show

main :: IO ()
main = do
  let nc = numCapabilities
  gnc <- getNumCapabilities
  gnp <- getNumProcessors
  ns <- numSparks
  mapM_
    (TIO.hPutStrLn stderr . tshow)
    [ ("numProcessors" :: Text, show nc)
    , ("getNumCapabilities" :: Text, show gnc)
    , ("getNumProcessors" :: Text, show gnp)
    , ("numSparks" :: Text, show ns)
    , ("rtsSupportsBoundThreads" :: Text, show rtsSupportsBoundThreads)
    ]

  doc <- do
    r <- get "https://www.destroyallsoftware.com/screencasts/feed"
    pure $ r ^. responseBody & Text.XML.parseLBS_ def
  let cursor = fromDocument doc
  let links = cursor $// element "item" &/ element "link" &/ content
  (bodies :: [(Text, Text)]) <- flip mapConcurrently links $ \link -> do
    r <- get (T.unpack link)
    pure (link, r ^. responseBody & BSL.toStrict & decodeUtf8)
  forM_ bodies $ \(_link, body) -> do
    let isMovie line = T.isInfixOf "source.src" line && T.isInfixOf ".mp4" line
    let movies = filter isMovie (T.lines body)
    let src1080p = case movies of
                     [_m4k, m1080p] -> m1080p
                     _              -> ""
    let split = T.splitOn "\"" src1080p
    case split of
      [_before, it, _after] -> TIO.putStrLn it
      _                     -> TIO.putStrLn $ "Bollocks: " <> tshow split
