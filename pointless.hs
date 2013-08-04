import Control.Arrow
import Control.Monad

average = uncurry (/) . foldr (uncurry (***) . ((+) *** (+) . const 1)) (0.0, 0.0) . join zip

xs = [2.0, 5.0, 7.0]

result = average xs
