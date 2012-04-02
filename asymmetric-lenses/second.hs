--
-- Second part of the paper.
--

--
-- 4. Representing an Asymmetric Lens in Scala
--

data CoState field record = CoState
  { get :: field
  , set :: field -> record
  }

-- A fused lens.
data FLens record field = FLens
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
