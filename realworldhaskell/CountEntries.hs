module CountEntries where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM, liftM)
import Control.Applicative

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  --where notDots path = path /= "." && path /= ".."
  where notDots = (&&) <$> (/= ".") <*> (/= "..") -- alternative (perhaps evil) implementation

countEntries :: FilePath -> IO [(FilePath, Int)]
countEntries path = do
  contents <- listDirectory path
  rest <- forM contents $ \ name -> do
    let newName = path </> name
    isDir <- doesDirectoryExist newName
    if isDir 
      then countEntries newName
      else return []
  return $ (path, length contents) : concat rest
