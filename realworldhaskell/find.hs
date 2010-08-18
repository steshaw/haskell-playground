--
-- Adapted from RWH Chapter 9 "I/O Case Study: A Library for Searching the Filesystem"
--

module Main where

import System.Environment (getArgs)
import Control.Monad (forM, forM_, when)
import System.Directory (doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

find :: FilePath -> IO [FilePath]
find topPath = do
  isDir <- doesDirectoryExist topPath
  if isDir 
    then do
      names <- getDirectoryContents topPath
      let properNames = filter (`notElem` [".", ".."]) names
      --let properNames = names -- keep "." and ".." if they appear
      paths <- forM properNames $ \ name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory then find path else return [path]
      return (topPath:(concat paths))
    else do
      isFile <- doesFileExist topPath
      if isFile
        then return [topPath]
        else error ("'" ++ topPath ++ "': no such file or directory")

main = do
{-
  getArgs >>= \args -> 
    case args of
      [path] -> find path >>= mapM_ putStrLn
-}
  args <- getArgs
  forM_ args $ \ path -> find path >>= mapM_ putStrLn
