module Main where

import Data.Set
import A
import B
import C

test :: Set (T U MB MC)
test = ins' (T X MB MC) $ ins (T X MB MC) $ ins (T Y MB MC) $ empty

main :: IO ()
main = print test
