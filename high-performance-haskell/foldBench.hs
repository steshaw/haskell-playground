import Criterion.Main
import Prelude hiding (foldl)

foldl, foldl' :: (a -> b -> a) -> a -> [b] -> a

foldl f z []     = z
foldl f z (x:xs) = foldl f (f z x) xs

foldl' f z []     = z
foldl' f z (x:xs) = let y = f z x
                    in y `seq` foldl' f y xs

main = defaultMain 
  [ bench "foldl"  (whnf (foldl  (+) 0) [1..10000])
  , bench "foldl'" (whnf (foldl' (+) 0) [1..10000])
  ]
