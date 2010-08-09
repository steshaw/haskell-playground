module Main where

import InteractWith
import SplitLines
import Steshaw

main = interactWithMain (splitLines >.> unlines)
