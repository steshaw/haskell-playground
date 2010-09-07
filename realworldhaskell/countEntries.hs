module Main () where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Writer (tell, WriterT, execWriterT)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first)

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

countEntries' path = execWriterT (countEntries path)

eg1 :: FilePath -> IO [(String, Int)]
eg1 path = liftM (map (first ("dir: " ++))) $ liftM (take 2) (execWriterT (countEntries path))

main :: IO ()
main = countEntries' "." >>= mapM_ print
