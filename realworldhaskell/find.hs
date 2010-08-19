-- {-# LANGUAGE ScopedTypeVariables #-}
--
-- Adapted from RWH Chapter 9 "I/O Case Study: A Library for Searching the Filesystem"
--

--module Main where

import System.Environment (getArgs)
import Control.Monad (forM, forM_, filterM)
import System.Directory (
  Permissions(..), getPermissions, getModificationTime, doesFileExist, doesDirectoryExist, getDirectoryContents)
import System.FilePath (takeExtension, (</>))
import System.Time (ClockTime)
import Control.Exception (handle, IOException)
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

-- FIXME: Exception handling problems causing resource leaks.
getFileSizeBadly path = do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return size

getFileSize :: FilePath -> IO (OptionalSize)
getFileSize path = getFileSizeBadly path >>= \size -> return $ Just size

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

extensionP path perms size modifiedT = takeExtension (pathP path perms size modifiedT)

sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing _ = -1

findWithP args p = forM_ args $ \ path -> betterFind p path >>= mapM_ putStrLn

sizeEq      p path perms size modifiedT = (sizeP      path perms size modifiedT) == p
pathEq      p path perms size modifiedT = (pathP      path perms size modifiedT) == p
extensionEq p path perms size modifiedT = (extensionP path perms size modifiedT) == p

sizeGe n path perms size modifiedT = (sizeP path perms size modifiedT) >= n
sizeGt n path perms size modifiedT = (sizeP path perms size modifiedT) >  n

sizeLe n path perms size modifiedT = (sizeP path perms size modifiedT) <= n
sizeLt n path perms size modifiedT = (sizeP path perms size modifiedT) <  n

inK n = n * 1024
inM n = inK n * 1024
inG n = inM n * 1024

-- Find Haskell source files greather than 4k.
-- TODO: Change to search for Haskell source files > 300 lines.
customP path perms size modifiedT =
  extensionEq ".hs" path perms size modifiedT && sizeGt (inK 4) path perms size modifiedT

main = do
  args <- getArgs
  case args of
    "-dirs":args    -> findWithP args isDirectoryP
    "-1":args           -> findWithP args $ customP
    "-pathEq":path:args -> findWithP args $ pathEq $ path
    "-sizeEq":size:args -> findWithP args $ sizeEq $ read size
    "-sizeGe":size:args -> findWithP args $ sizeGe $ read size
    "-sizeGt":size:args -> findWithP args $ sizeGt $ read size
    "-sizeLe":size:args -> findWithP args $ sizeLe $ read size
    "-sizeLt":size:args -> findWithP args $ sizeLt $ read size
    otherwise       -> findWithP args trueP

-- NOTE: unused
firstMain =
  getArgs >>= \args ->
    case args of
      [path] -> find path >>= mapM_ putStrLn

-- NOTE: unused
oldMain = do
  args <- getArgs
  forM_ args $ \ path -> find path >>= mapM_ putStrLn

