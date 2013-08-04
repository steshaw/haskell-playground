import Control.Arrow
import Control.Monad

average :: [Double] -> Double
average = uncurry (/) . foldr (uncurry (***) . ((+) *** (+) . const 1)) (0.0, 0) . join zip

xs :: [Double]
xs = [2.0, 5.0, 7.0]

result :: Double
result = average xs
