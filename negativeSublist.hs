prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes xs = [xs] ++ prefixes (init xs)

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = [xs] ++ suffixes (tail xs)

subs :: [a] -> [[a]]
subs = filter (not . null) . concat . map suffixes . prefixes

negativeSubs :: (Num a, Ord a) => [a] -> [[a]]
negativeSubs as = filter (\bs -> (sum bs) < 0) $ subs as

sumNegSubs :: (Num a, Ord a) => [a] -> Int
sumNegSubs as = length $ negativeSubs as
