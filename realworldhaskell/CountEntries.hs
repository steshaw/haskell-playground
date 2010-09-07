module CountEntries where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer
import Control.Applicative

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  --where notDots path = path /= "." && path /= ".."
  where notDots = (&&) <$> (/= ".") <*> (/= "..") -- alternative (perhaps evil) implementation

countEntries :: FilePath -> WriterT [(FilePath, Int)] IO ()
countEntries path = do
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \ name -> do
    let newPath = path </> name
    isDir <- liftIO . doesDirectoryExist $ newPath
    if isDir 
      then countEntries newPath
      else return ()

