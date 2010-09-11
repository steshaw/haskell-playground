module ErrorHandling where

divBy :: Integral a => a -> [a] -> Maybe [a]
divBy _ [] = Just []
divBy _ (0:_) = Nothing
divBy numerator (denominator:xs) = 
  case divBy numerator xs of
    Nothing -> Nothing
    Just results -> Just ((numerator `div` denominator) : results)

divBy2 :: Integral a => a -> [a] -> Maybe [a]
divBy2 _ [] = Just []
divBy2 _ (0:_) = Nothing
divBy2 numerator (denominator:xs) = 
  divBy numerator xs >>= \results -> Just ((numerator `div` denominator) : results)

--divByM :: Integral a => a -> [a] -> Maybe [a]
divByM numerator denominators = map (numerator `safeDiv`) denominators
  where
    safeDiv _ 0 = Nothing
    safeDiv x y = Just $ x `div` y
