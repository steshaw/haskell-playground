import Prelude hiding (sequence, mapM)

-- generalise map on Lists
liftM :: Monad m => (a -> b) -> m a -> m b
liftM f mx = do x <- mx
                return $ f x

liftM' :: Monad m => (a -> b) -> m a -> m b
liftM' f mx = mx >>= return . f

-- generalise concat on Lists
join :: Monad m => m (m a) -> m a
join mmx = do mx <- mmx
              x <- mx
              return x

join' :: Monad m => m (m a) -> m a
--join' mmx = mmx >>= \mx -> mx >>= return
join' mmx = mmx >>= id

(>>) :: Monad m => m a -> m b -> m b
mx >> my = do _ <- mx
              y <- my
              return y

sequence         :: Monad m => [m a] -> m [a]
sequence []       = return []
sequence (mx:mxs) = do x <- mx
                       xs <- sequence mxs
                       return (x:xs)

-- another generalisation of map
mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM mf [] = return []
mapM mf (x:xs) = do b <- mf x
                    bs <- mapM mf xs
                    return (b : bs)

mapM' :: Monad m => (a -> m b) -> [a] -> m [b]
mapM' f = sequence . map f

-- generalise foldr (or foldl)
foldM :: Monad m => (a -> b -> m a) -> a -> [b] -> m a
foldM f a (x:xs) = do a <- f a x
                      foldM f a xs
                      
