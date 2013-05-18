import Data.List (sort)

isSeq :: (Num a, Ord a) => [a] -> Bool
isSeq [] = True
isSeq as = let bs = sort as
           in all (== 1) (zipWith (-) (tail bs) bs)
