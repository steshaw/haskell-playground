module MaybeT where

newtype MaybeT m a = MaybeT {
  runMaybeT :: m (Maybe a)
}

bindMT :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
t `bindMT` f = MaybeT $ do
  fred <- runMaybeT t
  case fred of
    Nothing -> return Nothing
    Just a  -> runMaybeT (f a)

bindMT2 :: (Monad m) => MaybeT m a -> (a -> MaybeT m b) -> MaybeT m b
t `bindMT2` f = MaybeT $ runMaybeT t >>= \fred ->
  maybe (return Nothing) (\a -> runMaybeT (f a)) fred
{-
  case fred of
    Nothing -> return Nothing
    Just a  -> runMaybeT (f a)
-}

returnMT :: (Monad m) => a -> MaybeT m a
returnMT a = MaybeT $ return (Just a)

failMT :: (Monad m) => String -> MaybeT m a
failMT s = MaybeT $ return Nothing

instance (Monad m) => Monad (MaybeT m) where
  return = returnMT
  (>>=) = bindMT2
  fail = failMT
