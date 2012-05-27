--
-- Simple example of STRef
--
-- Found at http://www.haskell.org/pipermail/haskell-cafe/2006-July/016806.html
-- 
import Data.STRef
import Control.Monad.ST

foreach         :: (Monad m) => [a] -> (a -> m b) -> m ()
foreach         = flip mapM_
-- Bryn Keller's foreach, but with type restrictions

fac             :: (Num a, Enum a) => a -> a
fac n           = runST (fac' n)

fac'            :: (Num a, Enum a) => a -> ST s a
fac' n          = do    r <- newSTRef 1
                        foreach [1..n] (\x -> modifySTRef r (*x))         
                        x <- readSTRef r
                        return x
