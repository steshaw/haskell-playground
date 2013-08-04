import Control.Arrow

summer = (+)
lengther = (+) . const 1
myapp (fa, fb) t = (fa *** fb) t
dup f a = f a a
bit xs = foldr (\a -> myapp ((summer *** lengther) a)) (0.0, 0.0) (dup zip xs)
average = bit >>> uncurry (/)

xs = [2.0, 5.0, 7.0]

result = average xs
