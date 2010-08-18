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
import System.IO.Error
import System.IO (stderr, hPutStrLn)

--import qualified Control.Exception as Ex
--import qualified Control.OldException as OldEx

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
      --let properNames = names -- keep "." and ".." if they appear
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

getFileSize :: FilePath -> IO (OptionalSize)
getFileSize path = return Nothing

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

main = do
  args <- getArgs
  case args of
    "-dirs":args -> forM_ args $ \ path -> betterFind isDirectoryP path >>= mapM_ putStrLn
    otherwise    -> forM_ args $ \ path -> betterFind trueP path >>= mapM_ putStrLn

-- NOTE: unused
firstMain =
  getArgs >>= \args ->
    case args of
      [path] -> find path >>= mapM_ putStrLn

-- NOTE: unused
oldMain = do
  args <- getArgs
  forM_ args $ \ path -> find path >>= mapM_ putStrLn

