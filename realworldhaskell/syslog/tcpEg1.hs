module Main () where

import SyslogTcpClient
import System.Environment (getArgs)

main = do
  args <- getArgs
  case args of
    [name] -> loopEg1 name
