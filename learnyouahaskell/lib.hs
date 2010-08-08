
ssMax = foldl1 max

ssReverse = foldl (flip (:)) []

ssProduct = foldl1 (*)

ssFilter p = foldr (\i acc -> if p i then i:acc else acc) []

ssHead = foldl1 (\acc _ -> acc)

ssLast = foldr1 (\_ acc -> acc)

ssMap f = foldr (\i acc -> f i:acc) []
