import Control.Arrow

summer = (+)
lengther = (+) . const 1
myapp (fa, fb) (a, b) = (fa a, fb b)
average xs = uncurry (/) (foldr (\a -> myapp ((summer *** lengther) a)) (0.0, 0.0) (zip xs xs))
