--
-- FIXME: Doesn't currently output the whole file. Stops at first empty line...
--

import Data.Iteratee (fileDriver)
import Data.Iteratee.Char (printLines)
import System (getArgs)
import System.IO (hPutStrLn, stderr)

main = do
  args <- getArgs
  case args of
    ["-h"] -> usage
    ["-?"] -> usage
    [file] -> cat file
    []     -> cat "/etc/hosts"
    _      -> usage

usage = hPutStrLn stderr "usage: cat [file]"

cat file = fileDriver printLines file
