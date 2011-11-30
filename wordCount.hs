--
-- Inspired by http://blog.joda.org/2011/11/guide-to-evaluating-fantom.html
--

import Control.Arrow ((&&&))
import Data.List (group, sort)
import Text.Printf
import System (getArgs)

(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

main :: IO ()
main = do
  [filename] <- getArgs
  contents <- readFile filename
  contents |> words |> sort |> group |> map (head &&& length) |> mapM_ (\(key, len) ->
    printf "%s %d\n" key len)
