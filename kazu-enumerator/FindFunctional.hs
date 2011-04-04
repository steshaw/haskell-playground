module FindFunctional where

import FindImperative (isSearchableDir, getValidContents)
import Control.Monad (forM)
import Control.Applicative ((<$>))
import System.FilePath ((</>))
import Data.List (isInfixOf)

getRecursiveContents :: FilePath -> IO [FilePath]
getRecursiveContents dir = do
  cnts <- map (dir </>) <$> getValidContents dir
  cnts' <- forM cnts $ \path -> do
    isDirectory <- isSearchableDir path
    if isDirectory
      then getRecursiveContents path
      else return [path]
  return . concat $ cnts'

grep :: String -> [FilePath] -> [FilePath]
grep pattern = filter (pattern `isInfixOf`)

findFunctional :: FilePath -> String -> IO ()
findFunctional dir pattern =
  grep pattern <$> getRecursiveContents dir
    >>= mapM_ putStrLn
