module Main where

import qualified System.Environment as Env
import qualified System.IO       as IO
import qualified System.IO.Error as IO
import qualified Control.Exception as Ex

readHandler :: IOError -> IO ()
readHandler e
  | IO.isDoesNotExistError e = putStrLn "File does not exist"
  | otherwise = do putStrLn "Error"; print e

main :: IO ()
main = do
  [fileName] <- Env.getArgs
  (do contents <- IO.readFile fileName 
      putStrLn (fileName ++ ":")
      putStrLn contents)
  `Ex.catch` readHandler
