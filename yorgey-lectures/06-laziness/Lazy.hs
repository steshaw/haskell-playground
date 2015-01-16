import Debug.Trace

f1 :: Maybe a -> [Maybe a]
f1 m = [m, m]

f2 :: Maybe a -> [a]
f2 Nothing  = []
f2 (Just x) = [x]
