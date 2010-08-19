-- {-# LANGUAGE ScopedTypeVariables #-}
--
-- Adapted from RWH Chapter 9 "I/O Case Study: A Library for Searching the Filesystem"
--

--module Main where

import System.Environment (getArgs)
import Control.Monad (forM, forM_, filterM)
import System.Directory (
  Permissions(..), getPermissions, getModificationTime, doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import System.Time (ClockTime)
import Control.Exception (handle, IOException)
--import System.IO.Error
import System.IO

slipperyGetDirectoryContents :: FilePath -> IO [FilePath]
slipperyGetDirectoryContents path =
  catch (getDirectoryContents path)
    (\e -> do let errMsg = show (e :: IOException)
              hPutStrLn stderr ("Warning: could not get directory contents '" ++ path ++ "': " ++ errMsg)
              return [])

find :: FilePath -> IO [FilePath]
find topPath = do
  isDir <- doesDirectoryExist topPath
  if isDir 
    then do
      names <- slipperyGetDirectoryContents topPath
      let properNames = filter (`notElem` [".", ".."]) names
      paths <- forM properNames $ \ name -> do
        let path = topPath </> name
        isDirectory <- doesDirectoryExist path
        if isDirectory then find path else return [path]
      return (topPath:(concat paths))
    else do
      hPutStrLn stderr $ "check if file exists " ++ topPath
      isFile <- doesFileExist topPath
      if isFile
        then return [topPath]
        else error ("'" ++ topPath ++ "': no such file or directory")

type OptionalSize = Maybe Integer
type LastModified = ClockTime
type Predicate = FilePath -> Permissions -> OptionalSize -> LastModified -> Bool

getFileSizeBadly path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

getFileSize :: FilePath -> IO (OptionalSize)
getFileSize path = getFileSizeBadly path >>= \size -> return (Just size)

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = find path >>= filterM check
  where check name = handle (\e -> do
              let errMsg = show (e :: IOException)
              hPutStrLn stderr ("Warning: could not get permissions '" ++ path ++ "': " ++ errMsg)
              return False)
           (do perms <- getPermissions name
               size <- getFileSize name
               modified <- getModificationTime name
               return (p name perms size modified))

isDirectoryP :: Predicate
isDirectoryP filePath perms size lastModified = searchable perms

trueP :: Predicate
trueP _ _ _ _ = True

pathP path _ _ _ = path

sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

findWithP args p = forM_ args $ \ path -> betterFind p path >>= mapM_ putStrLn

sizeGe geSize path perms size modifiedT = (sizeP path perms size modifiedT) > geSize

main = do
  args <- getArgs
  case args of
    "-dirs":args -> findWithP args isDirectoryP
    "-1":args    -> findWithP args $ sizeGe 1024
    otherwise    -> findWithP args trueP

-- NOTE: unused
firstMain =
  getArgs >>= \args ->
    case args of
      [path] -> find path >>= mapM_ putStrLn

-- NOTE: unused
oldMain = do
  args <- getArgs
  forM_ args $ \ path -> find path >>= mapM_ putStrLn

