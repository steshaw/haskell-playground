--
-- Inspired by http://blog.joda.org/2011/11/guide-to-evaluating-fantom.html
--

import System (getArgs)
import Data.List (group, sort)
import Text.Printf

groupWords :: String -> [[String]]
groupWords = group . sort . words

printWords :: String -> IO ()
printWords = (mapM_ output) . groupWords
  where output g = printf "%s %d\n" (g !! 0) (length g)

main :: IO ()
main = getArgs >>= (readFile . head) >>= printWords
