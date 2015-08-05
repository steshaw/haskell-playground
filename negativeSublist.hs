prefixes :: [a] -> [[a]]
prefixes [] = [[]]
prefixes xs = [xs] ++ prefixes (init xs)

suffixes :: [a] -> [[a]]
suffixes [] = [[]]
suffixes xs = [xs] ++ suffixes (tail xs)

subs :: [a] -> [[a]]
subs = filter (not . null) . concat . map suffixes . prefixes

negativeSubs :: (Num a, Ord a) => [a] -> [[a]]
negativeSubs = filter (\bs -> (sum bs) < 0) . subs

sumNegSubs :: (Num a, Ord a) => [a] -> Int
sumNegSubs = length . negativeSubs
