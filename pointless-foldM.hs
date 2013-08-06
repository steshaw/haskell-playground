{-# LANGUAGE NoMonomorphismRestriction #-}

import Control.Arrow
import Control.Monad
import Control.Monad.Identity

import Prelude hiding (length, sum)

data Hole = Hole

length :: Monad m => [Double] -> m Integer
length xs = foldM f 0 xs
  where f = \i d -> do return $ i + 1

sum :: Monad m => [Double] -> m Double
sum xs = foldM f 0 xs
  where f = \a d -> do return $ a + d

averageBits :: Monad m => [Double] -> m (Double, Integer)
averageBits xs = foldM f (0.0, 0) (map (id &&& (const 1)) xs)
  where 
--    f :: (Double, Integer) -> (Double, Integer) -> m (Double, Integer)
    f (accSum, accLen) (d, i) = do return $ (accSum + d, accLen + i)

average xs = liftM (uncurry (/) . second fromInteger) (averageBits xs)

xs :: [Double]
xs = [2.0, 5.0, 7.0]

l = length xs
s = sum xs
ab = averageBits xs
