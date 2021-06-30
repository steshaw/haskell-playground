#!/usr/bin/env stack
{-
  stack script --resolver lts-18.0
    --package containers
    --package hscolour
    --package mime-types
    --package pretty-show
    --package text
-}
{-# language OverloadedStrings #-}

import Data.Map.Strict
import Network.Mime

import Text.Show.Pretty (ppShow)
import System.IO as IO
import Language.Haskell.HsColour
import Language.Haskell.HsColour.Colourise

colorPrint thing = do
  colourPrefs <- readColourPrefs
  -- HT http://teh.id.au/posts/2017/02/13/interactive-print/index.html
  IO.putStrLn . hscolour TTY colourPrefs False False "" False . ppShow $ thing

mimeTypes = ["doc", "docx", "pdf", "rtf", "txt"]

main = do
  let mimeMap = filterWithKey (\k _ -> k `elem` mimeTypes) defaultMimeMap
  colourPrefs <- readColourPrefs
  colorPrint ("colourPrefs", colourPrefs)
  colorPrint ("defaultColourPrefs", defaultColourPrefs)
  colorPrint $ toList mimeMap
