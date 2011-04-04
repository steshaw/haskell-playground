--
-- From http://leimy9.blogspot.com/2010/02/iteratee.html
--
module Main where

import Control.Monad.Trans
import Data.Iteratee.Base
import Data.Iteratee.Base.StreamChunk (ReadableChunk (..))
import Data.Iteratee.IO.Handle
import System.Process
import System.IO

-- For some reason this signature is wrong, but I'm not sure why...
--handleDriver :: (MonadIO m, ReadableChunk s el) => IterateeG s el m a -> Handle -> m a
handleDriver iter h = do
  result <- enumHandle h iter >>= run
  liftIO $ hClose h
  return result

main :: IO ()
main = do
  (_, outp, _, _) <- runInteractiveCommand "/bin/cat /etc/passwd"
  handleDriver (stream2list :: IterateeG [] Char IO String) outp >>= putStrLn
