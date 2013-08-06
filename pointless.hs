import Control.Arrow
import Control.Monad

average :: [Double] -> Double
average = uncurry (/) . foldl (uncurry (***) . ((+) *** flip ((+) . const 1))) (0.0, 0) . join zip

averageBits :: [Double] -> (Double, Integer)
averageBits xs = foldl f (0.0, 0) (map (id &&& (const 1)) xs)
  where 
    f :: (Double, Integer) -> (Double, Integer) -> (Double, Integer)
    f (accSum, accLen) (d, i) = (accSum + d, accLen + i)

average' xs = (uncurry (/) . second fromInteger) (averageBits xs)

xs :: [Double]
xs = [2.0, 5.0, 7.0]

result :: Double
result = average xs
