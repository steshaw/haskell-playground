--
-- Simple example of STRef
--
-- Adapted from http://www.haskell.org/pipermail/haskell-cafe/2006-July/016806.html
-- 
import Data.STRef
import Control.Monad.ST
import Control.Monad (forM_)

fac             :: (Num a, Enum a) => a -> a
fac n           = runST (fac' n)

fac'            :: (Num a, Enum a) => a -> ST s a
fac' n          = do    r <- newSTRef 1
                        forM_ [1..n] (\x -> modifySTRef r (*x))         
                        x <- readSTRef r
                        return x
