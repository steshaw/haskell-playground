import Prelude hiding ((==), Eq, Monad, return)

class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x /= y = not (x == y)

instance Eq Bool where
  False == False = True
  True  == True  = True
  _     == _     = False

class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b

instance Monad Maybe where
--  return :: a -> Maybe a
  return = Just

--  (>>=)         :: Maybe a -> (a -> Maybe b) -> Maybe b
  Nothing  >>= _ = Nothing
  (Just x) >>= f = f x

instance Monad [] where
  return x = [x]
  xs >>= f = concat (map f xs)

pairs :: [a] -> [b] -> [(a, b)]
pairs xs ys = do x <- xs
                 y <- ys
                 return (x, y)
