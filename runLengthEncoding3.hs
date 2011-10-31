--
-- Adapted from http://rosettacode.org/wiki/Run-length_encoding#Haskell
--

import System (getArgs)
import Control.Arrow ((>>>), (&&&))
import Data.List (group)
 
type Encoded = [(Int, Char)]
type Decoded = String
 
-- Takes a decoded string and returns an encoded list of tuples
rle :: Decoded -> Encoded
rle = group >>> map (length &&& head)

-- stringify the encoded result (like the examples at Rosetta Code).
rleString :: Encoded -> String
rleString es = concat $ showTuple `map` es
  where
    showTuple :: (Int, Char) -> String
    showTuple (n, c) = (show n) ++ [c]
 
main :: IO ()
main = do
  (s : []) <- getArgs
  putStrLn $ show $ rleString $ rle s
