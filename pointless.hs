import Control.Arrow

summer = (+)
lengther = (+) . const 1
myapp = uncurry (***)
dup f a = f a a
bit = foldr (myapp . (summer *** lengther)) (0.0, 0.0)
average = (uncurry (/) . bit . dup zip)

xs = [2.0, 5.0, 7.0]

result = average xs
