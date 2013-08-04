import Control.Arrow

dup f a = f a a
bit = foldr (uncurry (***) . ((+) *** (+) . const 1)) (0.0, 0.0)
average = uncurry (/) . bit . dup zip

xs = [2.0, 5.0, 7.0]

result = average xs
