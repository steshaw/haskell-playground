module Main where

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Control.Monad (forM_, liftM, forM_)
import Control.Monad.Trans (lift, liftIO)
import Control.Monad.Reader (ask, ReaderT, runReaderT)
import Control.Monad.State (get, put, StateT, runStateT)
import Control.Monad.Writer (tell, WriterT, execWriterT)
import Control.Applicative ((<$>), (<*>))
import Control.Arrow (first, second)

data AppConfig = AppConfig {
  cfgMaxDepth :: Integer
} deriving (Show)

data AppState = AppState {
  stDepth :: Integer,
  stMaxDepth :: Integer
} deriving (Show)

type App = ReaderT AppConfig (StateT AppState IO)

listDirectory :: FilePath -> IO [String]
listDirectory = liftM (filter notDots) . getDirectoryContents
  --where notDots path = path /= "." && path /= ".."
  where notDots = (&&) <$> (/= ".") <*> (/= "..") -- alternative (perhaps evil) implementation

countEntries :: FilePath -> WriterT [(FilePath, Int)] App ()
countEntries path = do
  cfg <- ask
  contents <- liftIO . listDirectory $ path
  tell [(path, length contents)]
  forM_ contents $ \ name -> do
    let newPath = path </> name
    isDir <- liftIO . doesDirectoryExist $ newPath
    st <- get
    if isDir && stDepth st < cfgMaxDepth cfg
      then do
        let newDepth = stDepth st + 1
        put st {stDepth = newDepth, stMaxDepth = max (stMaxDepth st) newDepth}
        countEntries newPath
      else
        return ()

countEntries' path = execWriterT (countEntries path)

{-
eg1 :: FilePath -> IO [(String, Int)]
eg1 path = liftM (map (first ("dir: " ++))) $ liftM (take 2) (countEntries' path)
-}

countEntries'' maxDepth path = 
  runStateT (runReaderT (countEntries' path) AppConfig {cfgMaxDepth = maxDepth}) AppState {stDepth = 0, stMaxDepth = 0}

data Result a = Result {
  depth :: Integer, 
  entries :: a
} deriving (Show)

main :: IO ()
main = forM_ [0..3] $ \depth ->
  countEntries'' depth "." >>= \(r, _) -> print $ Result depth r
