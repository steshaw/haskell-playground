--
-- Prompted by discussion at:
-- http://stackoverflow.com/questions/21208771/exception-handling-for-readfile
--
module Main where

import qualified Control.Exception as Ex
import Prelude hiding (readFile)
import qualified System.Environment as Env
import qualified System.IO as IO
import qualified System.IO.Error as IO

readFile
    :: Ex.Exception ex
    => IO.FilePath -> IO (Either ex String)
readFile fileName = Ex.try (IO.readFile fileName)

readHandler :: IO.IOError -> IO ()
readHandler e
    | IO.isDoesNotExistError e = putStrLn "File does not exist"
    | otherwise = do
        putStrLn "Error"
        print e

main :: IO ()
main = do
    [fileName] <- Env.getArgs
    x <- readFile fileName
    case x of
        Left e -> readHandler e
        Right contents -> do
            putStrLn (fileName ++ ":")
            putStrLn contents
