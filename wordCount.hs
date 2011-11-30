--
-- Inspired by http://blog.joda.org/2011/11/guide-to-evaluating-fantom.html
--

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import Data.List (group, sort)
import Text.Printf
import System (getArgs)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  let g = (group . sort . words) contents
  let f = map (head &&& length) g
  f `forM_` (\(key, len) -> printf "%s %d\n" key len)
