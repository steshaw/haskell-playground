
(=<<) :: Monad m => (a -> m b) -> m a -> m b

(>>=) :: Monad m => m a -> (a -> m b) -> m b

(>>)  :: Monad m => m a -> m b -> m b

