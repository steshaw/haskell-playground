--
-- Used by Simon Marlow to test GC changes to help mutable arrays.
--
--   http://hackage.haskell.org/trac/ghc/ticket/650
--

import Control.Monad
import qualified Data.HashTable as H
import System.Environment

main :: IO ()
main = do
  [size] <- fmap (fmap read) getArgs
  m <- H.new (==) H.hashInt
  forM_ [1..size] $ \n -> H.insert m n n
  v <- H.lookup m 100
  print v
