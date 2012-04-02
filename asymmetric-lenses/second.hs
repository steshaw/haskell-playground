{-# LANGUAGE RankNTypes #-}

--
-- Second part of the paper.
--

--
-- 4. Representing an Asymmetric Lens in Scala
--

-- A fused lens.
newtype FLens record field = FLens
  { apply :: record -> CoState field record
  }

-- Strange way to encoding the State Monad.
newtype State s a = State { run :: s -> (a, s) }

mapState :: (a -> b) -> State s a -> State s b
mapState f sm = State
  { run = \s -> let (a, t) = (run sm) s in (f a, t)
  }

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
flatMapState :: State s a -> (a -> State s b) -> State s b
flatMapState sm f = State
  { run = \s -> let (a, t) = (run sm) s in run (f a) t
  }

data CoState field record = CoState
  { get :: field
  , set :: field -> record
  }

-- mapCoState :: (a -> b) -> CoState s a -> CoState s b
mapCoState :: (r -> s) -> CoState f r -> CoState f s
mapCoState f csm = CoState
  { get = get csm
  , set = f . (set csm)
  }

-- (>>=) :: Monad m => m a -> (a -> m b) -> m b
-- extend :: (w a -> b) -> w a -> w b
coFlatMap :: CoState f r -> (CoState f r -> s) -> CoState f s
coFlatMap csm f = CoState 
  { get = get csm
  , set = \k -> f $ CoState {get = k, set = (set csm)}
  }

-- join :: Monad m => m (m a) -> m a
coFlatten :: forall f r. (CoState f) r -> (CoState f) ((CoState f) r)
coFlatten csm = coFlatMap csm id
