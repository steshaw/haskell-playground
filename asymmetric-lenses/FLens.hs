{-# LANGUAGE RankNTypes #-}
module FLens where

import Person
import qualified Data.Map as M

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

--
-- Get the example working again with FLens
--

addressL :: FLens Person Address
addressL = FLens (\p -> CoState
  { get = address p
  , set = \a -> p {address = a}
  })

streetL :: FLens Address String
streetL = FLens (\p -> CoState
  { get = street p
  , set = \a -> p {street = a}
  })

stateL :: FLens Address String
stateL = FLens (\p -> CoState 
  { get = state p
  , set = \a -> p {state = a}
  })

composeL :: FLens b c -> FLens a b -> FLens a c
composeL l1 l2 = FLens (\r -> CoState
  { get = get ((apply l1) (get ((apply l2) r)))
  , set = \f -> let csm2 = apply l2 r 
                in set csm2 (set (apply l1 (get csm2)) f)
  })

mapL :: Ord k => k -> FLens (M.Map k v) (Maybe v)
mapL k = FLens (\m -> CoState
  { get = M.lookup k m
  , set = \v -> case v of
      Nothing -> M.delete k m
      Just w  -> M.insert k w m
  })

personStreetL = streetL `composeL` addressL
streetOfPerson = get (apply personStreetL person)
newPerson = set (apply personStreetL person) "Elizabeth Street"
