--
-- Adapted from RWH Chapter 9 "I/O Case Study: A Library for Searching the Filesystem"
--

module Main where

import System.Environment (getArgs)
import Control.Monad (forM)
import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))

find :: FilePath -> IO [FilePath]
find topDir = do
  names <- getDirectoryContents topDir
  let properNames = filter (`notElem` [".", ".."]) names
  paths <- forM properNames $ \ name -> do
    let path = topDir </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then find path else return [path]
  return (concat paths)

main = do
  getArgs >>= \args -> 
    case args of
      [path] -> find path >>= mapM_ putStrLn
