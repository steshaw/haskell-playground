module FindImperative where

import Control.Monad (forM_, when)
import Control.Applicative ((<$>), (<*>))
import System.Directory (getDirectoryContents, getPermissions, searchable, doesDirectoryExist)
import System.FilePath ((</>))
import Data.List (isInfixOf)

getValidContents :: FilePath -> IO [String]
getValidContents path =
  filter (`notElem` [".", "..", ".git", ".svn", ".hg", ".bzr"])
    <$> getDirectoryContents path

isSearchableDir :: FilePath -> IO Bool
isSearchableDir dir =
  (&&) <$> doesDirectoryExist dir
       <*> (searchable <$> getPermissions dir)

findImperative :: FilePath -> String -> IO ()
findImperative dir pattern = do
  cnts <- map (dir </>) <$> getValidContents dir
  forM_ cnts $ \path -> do
    when (pattern `isInfixOf` path) $ putStrLn path
    isDirectory <- isSearchableDir path
    when isDirectory $ findImperative path pattern
