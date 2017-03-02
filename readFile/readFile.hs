--
-- Prompted by discussion at:
-- http://stackoverflow.com/questions/21208771/exception-handling-for-readfile
--
module Main where

import qualified Control.Exception as Ex
import qualified Text.Printf as Text
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Prelude hiding (readFile, FilePath)
import qualified System.Environment as Env
import qualified System.IO.Error as IO

type FilePath = Text.Text

readFile
    :: Ex.Exception ex
    => FilePath -> IO (Either ex Text.Text)
readFile fileName = Ex.try (TextIO.readFile (Text.unpack fileName))

getArgs :: IO [Text.Text]
getArgs = do
    args <- Env.getArgs
    pure $ map Text.pack args

readHandler :: IO.IOError -> IO ()
readHandler e
    | IO.isDoesNotExistError e = putStrLn "File does not exist"
    | otherwise = do
        putStrLn "Error"
        print e

main :: IO ()
main = do
    [fileName] <- getArgs
    x <- readFile fileName
    case x of
        Left e -> readHandler e
        Right contents -> do
            Text.printf "%s:\n" fileName
            TextIO.putStrLn contents
