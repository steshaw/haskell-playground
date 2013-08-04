import Control.Arrow

lengther = (+) . const 1
dup f a = f a a
bit = foldr (uncurry (***) . ((+) *** lengther)) (0.0, 0.0)
average = uncurry (/) . bit . dup zip

xs = [2.0, 5.0, 7.0]

result = average xs
