module FindIteratee where

import FindImperative (getValidContents, isSearchableDir)

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.Enumerator hiding (map, filter, filterM)
import qualified Data.Enumerator.List as EL
import Data.List
import System.Directory
import System.FilePath

grepE :: String -> Enumeratee String String IO b
grepE pattern = EL.filter (pattern `isInfixOf`)

printI :: Iteratee String IO ()
printI = do
  mx <- EL.head
  case mx of
    Nothing -> return ()
    Just file -> do
      liftIO . putStrLn $ file
      printI

enumDir :: FilePath -> Enumerator String IO b
enumDir dir = list
  where
    list (Continue k) = do
      (files, dirs) <- liftIO getFilesDirs
      if null dirs
        then k (Chunks files)
        else k (Chunks files) >>== walk dirs
    list step = returnI step
    walk dirs = foldr1 (<==<) $ map enumDir dirs
    getFilesDirs = do
      cnts <- map (dir </>) <$> getValidContents dir
      (,) <$> filterM doesFileExist cnts
          <*> filterM isSearchableDir cnts

findEnum :: FilePath -> String -> IO ()
findEnum dir pattern = run_ $ 
  enumDir dir $$ (grepE pattern =$ printI)

findEnumLimit :: FilePath -> String -> Integer -> IO ()
findEnumLimit dir pattern n = run_ $ enumDir dir $$ (grepE pattern =$ (EL.isolate n =$ printI))
