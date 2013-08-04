import Control.Arrow

summer = (+)
lengther = (+) . const 1
myapp = uncurry (***)
dup f a = f a a
bit xs = foldr (myapp . (summer *** lengther)) (0.0, 0.0) (dup zip xs)
average = bit >>> uncurry (/)

xs = [2.0, 5.0, 7.0]

result = average xs
