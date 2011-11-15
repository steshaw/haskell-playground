class Lift f where
  lift0 ::
    a -> f a
 
  lift1 ::
    (a -> b) -> f a -> f b
  lift1 =
    ap . lift0
 
  lift2 ::
    (a -> b -> c) -> f a -> f b -> f c
  lift2 f =
    ap . lift1 f
 
  lift3 ::
    (a -> b -> c -> d) -> f a -> f b -> f c -> f d
  lift3 f a =
    ap . lift2 f a
 
  ap ::
    f (a -> b) -> f a -> f b
 
-- scala.List
instance Lift [] where
  lift0 =
    error "todo"
 
  ap = 
    error "todo"
 
-- scala.Option
instance Lift Maybe where
  lift0 =
    error "todo"
 
  ap = 
    error "todo"
 
-- scala.Either[R, _]
instance Lift (Either r) where
  lift0 =
    error "todo"
 
  ap = 
    error "todo"
 
-- scala.Functior1[R, _]
instance Lift ((->) r) where
  lift0 =
    error "todo"
 
  ap = 
    error "todo"
