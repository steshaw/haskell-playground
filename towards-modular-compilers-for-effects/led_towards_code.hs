--
-- Downloaded from http://www.cs.nott.ac.uk/~led/code/led_towards_code.hs
--
-- See also:
--
--   Slides http://www.cs.nott.ac.uk/~led/talks/led_modularcompilers_slides.pdf
--   Paper  http://www.cs.nott.ac.uk/~led/papers/led_towards_tfp11.pdf
--

{----------------------------------------------------------------------------

                   Towards Modular Compilers for Effects

                     Laurence E. Day and Graham Hutton
                     Functional Programming Laboratory
                        School of Computer Science
                       University of Nottingham, UK

                                 June 2011

----------------------------------------------------------------------------}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Prelude hiding (catch, (>>))
import Maybe (fromJust)
import Control.Monad (liftM)

{-==== FIXPOINT / FOLDING / COPRODUCT ====-}

data Fix f = In (f (Fix f))

out :: Fix f -> f (Fix f)
out (In x) = x

fold :: Functor f => (f a -> a) -> Fix f -> a
fold f (In t) = f $ fmap (fold f) t

infixr 5 :+:
data (f :+: g) e = Inl (f e) | Inr (g e)

{-==== THE LANGUAGES ====-}

data Arith   e = Val Int | Add e e
data Except  e = Throw | Catch e e
type Expr      = Fix (Arith :+: Except)

data EMPTY   e = NULL

data ARITH   e = PUSH Int e | ADD e
data EXCEPT  e = MARK Code e | UNMARK e | THROW e
type Code     = Fix (ARITH :+: EXCEPT :+: EMPTY)

data Integ   e = VAL Int e
data Handler e = HAND Code e
type Stack     = Fix (Integ :+: Handler :+: EMPTY)

type Value = Int

{-==== DECLARING EVERYTHING AS A FUNCTOR ====-}

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl x) = Inl (fmap f x)
  fmap g (Inr y) = Inr (fmap g y)
  
instance Functor Arith where
  fmap _ (Val n)   = Val n
  fmap f (Add x y) = Add (f x) (f y)
  
instance Functor Except where
  fmap f (Throw)     = Throw
  fmap f (Catch x h) = Catch (f x) (f h)
  
instance Functor ARITH where
  fmap f (PUSH n e) = PUSH n (f e)
  fmap f (ADD e)    = ADD (f e)
  
instance Functor EXCEPT where
  fmap f (MARK c e) = MARK c (f e)
  fmap f (UNMARK e) = UNMARK (f e)
  fmap f (THROW e)  = THROW (f e)
  
instance Functor Integ where
  fmap f (VAL n e)  = VAL n (f e)
  
instance Functor Handler where
  fmap f (HAND c e) =  HAND c (f e)
  
instance Functor EMPTY where
  fmap _ NULL = NULL
 
{-==== SUBTYPING RELATION ====-}
  
class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a
  prj :: sup a -> Maybe (sub a)
  
instance Functor f => (:<:) f f where
  inj = id
  prj = Just
  
instance (Functor f, Functor g) => (:<:) f (f :+: g) where
  inj = Inl
  prj (Inl x) = Just x
  prj (Inr _) = Nothing
  
instance (Functor f, Functor g, Functor h, (:<:) f h) => (:<:) f (g :+: h) where
  inj = Inr . inj
  prj (Inl _) = Nothing
  prj (Inr y) = prj y
  
match        :: (g :<: f) => Fix f -> Maybe (g (Fix f))
match (In t) =  prj t

{-==== SMART CONSTRUCTORS ====-}
  
inject    :: (g :<: f) => g (Fix f) -> Fix f
inject    =  In . inj

val       :: (Arith :<: f) => Int -> Fix f
val n     =  inject $ Val n

add       :: (Arith :<: f) => Fix f -> Fix f -> Fix f
add x y   =  inject $ Add x y

throw     :: (Except :<: f) => Fix f
throw     =  inject Throw

catch     :: (Except :<: f) => Fix f -> Fix f -> Fix f
catch x h =  inject $ Catch x h

---- COMPILER CONSTRUCTORS ----

pushc       :: Int -> Code -> Code
n `pushc` c = inject $ PUSH n c

addc        :: Code -> Code
addc c      =  inject $ ADD c

throwc      :: Code -> Code
throwc c    =  inject $ THROW c

markc       :: Code -> Code -> Code
h `markc` c =  inject $ MARK h c

unmarkc     :: Code -> Code
unmarkc c   =  inject $ UNMARK c

emptycode   :: Code
emptycode   = inject NULL

--- EXECUTOR CONSTRUCTORS ----

intstack        :: Int -> Stack -> Stack
n `intstack` s  =  inject $ VAL n s

handstack       :: Code -> Stack -> Stack
h `handstack` s =  inject $ HAND h s

emptystack      :: Stack
emptystack      =  inject NULL

{-==== THE COMPILER ====-}
  
class Functor f => Comp f where
  compAlg :: f (Code -> Code) -> Code -> Code

instance (Comp f, Comp g) => Comp (f :+: g) where
  compAlg (Inl x) = compAlg x
  compAlg (Inr y) = compAlg y
  
instance Comp Arith where
  compAlg (Val n) = pushc n
  compAlg (Add x y) = x . y . addc
  
instance Comp Except where
  compAlg (Throw) = throwc
  compAlg (Catch x h) = \c -> h c `markc` (x $ unmarkc c)

comp :: (Comp f) => Fix f -> Code
comp e = fold compAlg e emptycode

{-==== EXAMPLE EXPRESSIONS ====-}

ex1 :: Expr
ex1 = add (val 1) (val 2)

ex2 :: Expr
ex2 = catch (add (val 1) throw) (val 7)

ex3 :: Expr
ex3 = catch (add (val 1) (val 2)) (val 7)

ex4 :: Expr
ex4 = throw

ex5 :: Expr
ex5 = catch (throw) (catch (val 1337) (throw))
  
{-==== MONAD DECLARATIONS/INSTANTIATIONS ====-}
  
newtype ErrorT m a   = ErrorT { runError :: m (Maybe a) }
newtype StateT s m a = StateT { runState :: s -> m (a, s) }

type StackTrans m a = StateT Stack m a

(>>) :: Monad m => m a -> m b -> m b
f >> g = f >>= \_ -> g
                     
instance Monad m => Monad (ErrorT m) where
  return = ErrorT . return . Just
  (ErrorT x) >>= f = ErrorT $ do x' <- x
                                 case x' of
                                   Nothing -> return Nothing
                                   Just y  -> runError $ f y
                                   
instance Monad m => Monad (StateT s m) where
  return a         = StateT $ \s -> return (a, s)
  (StateT x) >>= f = StateT $ \s -> do (a, t) <- x s
                                       (b, u) <- runState (f a) t
                                       return (b, u)
  
{-==== MONAD TYPECLASSES ====-}
                                   
class Monad m => ErrorMonad m where
  throwError :: m a
  catchError :: m a -> m a -> m a

instance Monad m => ErrorMonad (ErrorT m) where
  throwError       = ErrorT $ return Nothing
  x `catchError` h = ErrorT $ do x' <- runError x
                                 case x' of
                                   Nothing -> runError h
                                   Just _  -> return x'
                                   
instance ErrorMonad Maybe where
  throwError             = Nothing
  Nothing `catchError` h = h
  x `catchError` _       = x

{-==== MONAD TRANSFORMERS ====-}

class MonadT t where
  lift :: (Monad m) => m a -> t m a 
  
instance MonadT (ErrorT) where
  lift m = ErrorT $ do a <- m
                       return $ Just a

{-==== THE VIRTUAL MACHINE ====-}

class (Monad m, Functor f) => Exec f m where
  execAlg :: f (StackTrans m ()) -> StackTrans m ()
  
instance (Exec f m, Exec g m) => Exec (f :+: g) m where
  execAlg (Inl x) = execAlg x
  execAlg (Inr y) = execAlg y
  
exec :: (Monad m, Exec f m) => Fix f -> StackTrans m ()
exec = fold execAlg

instance Monad m => Exec ARITH m where
  execAlg (PUSH n st) = pushs n >> st
  execAlg (ADD st)    = adds >> st
  
instance (ErrorMonad m) => Exec EXCEPT m where
  execAlg (THROW _)   = unwinds
  execAlg (MARK h st) = marks h >> st
  execAlg (UNMARK st) = unmarks >> st

instance Monad m => Exec EMPTY m where
  execAlg NULL = return ()
  
{-==== THE EXTRACTOR ====-}
  
dropVoid :: Monad m => m (a, Stack) -> m Stack
dropVoid m = m >>= \(void, stack) -> return stack
  
class Functor f => Extr f where
  extrAlg :: f Value -> Value
  
instance (Extr f, Extr g) => Extr (f :+: g) where
  extrAlg (Inl x) = extrAlg x
  extrAlg (Inr y) = extrAlg y
  
instance Extr Integ where
  extrAlg (VAL n _)  = n
  
instance Extr Handler where
  extrAlg (HAND _ _) = error "Incomplete Computation"
  
instance Extr EMPTY where
  extrAlg NULL       = error "Computation Failed"

extract' :: Extr f => Fix f -> Value
extract' = fold extrAlg

extractM :: (Monad m, Extr f) => m (Fix f) -> m Value
extractM = liftM extract'

extract :: (ErrorMonad m, Comp f) => Fix f -> m Value 
extract x = extractM $ dropVoid $ runState (exec $ comp x) emptystack

{-==== AUXILIARY FUNCTIONS ====-}

stackShift :: Monad m => (s -> s) -> StateT s m s
stackShift f = StateT $ \s -> return (s, f s)

pushs n = stackShift (\st -> inject $ VAL n st)  >> return ()
marks h = stackShift (\st -> inject $ HAND h st) >> return ()

adds :: Monad m => StackTrans m ()
adds =  do x <- pop
           y <- pop
           let extract :: Stack -> Integ Stack = fromJust . match in
            case (extract x, extract y) of
             (VAL n _, VAL m _) -> pushs (n + m)

unwinds :: ErrorMonad m => StackTrans m ()
unwinds =  do b <- isEmpty
              case not b of
                True -> unwinds'
                _    -> error "Attempting to unwind an empty stack."
                   
unwinds' :: ErrorMonad m => StackTrans m ()
unwinds' =  pop >>= \x -> case match x of
                           Just (HAND h _) -> exec h
                           _               -> unwinds

unmarks :: Monad m => StackTrans m ()
unmarks =  stackShift modunmark >> return ()

isEmpty :: Monad m => StackTrans m Bool
isEmpty = StateT $ \s -> case match s of
                          (Just NULL) -> return (True, s)
                          _           -> return (False, s)

pop :: Monad m => StackTrans m Stack
pop =  stackShift modtail >>= return . modhead

{-==== "MODULAR" STACK OPERATIONS ====-}

class ModularStack f where
  modtail   :: f  -> Stack
  modhead   :: f  -> Stack
  modunmark :: f  -> Stack
  
instance ModularStack Stack where
  modtail   (In x) = modtail x
  modhead   (In x) = modhead x
  modunmark (In x) = modunmark x
  
instance (ModularStack (f Stack), ModularStack (g Stack)) => 
         ModularStack ((f :+: g) Stack) where
  modtail   (Inl x) = modtail x
  modtail   (Inr y) = modtail y
  modhead   (Inl x) = modhead x
  modhead   (Inr y) = modhead y
  modunmark (Inl x) = modunmark x
  modunmark (Inr y) = modunmark y
  
instance ModularStack (Integ Stack) where
  modtail   (VAL _ st) = st
  modhead   (VAL n _)  = n `intstack` emptystack
  modunmark (VAL n st) = n `intstack` modunmark st
  
instance ModularStack (Handler Stack) where
  modtail   (HAND _ st) = st
  modhead   (HAND h _)  = h `handstack` emptystack
  modunmark (HAND h st) = st
  
instance ModularStack (EMPTY e) where
  modtail   _ = emptystack
  modhead   _ = emptystack
  modunmark _ = emptystack
  
{-==== SHOWING EVERYTHING ====-}
  
--- SHOWING COPRODUCTS ---
  
instance (Show (f e), Show (g e)) => Show ((f :+: g) e) where
  show (Inl x) = "Inl ("++show x++")"
  show (Inr y) = "Inr ("++show y++")"
  
  --instance Show (f e) => Show (Fix f) where
  
--- SHOWING CODE ---
  
instance Show Code where
  show (In x) = "In (" ++ show x ++ ")"
  
instance (Show e) => Show (ARITH e) where
  show (PUSH n e) =  "Push " ++ show n ++ ", " ++ show e
  show (ADD e)    =  "Add, " ++ show e
  
instance (Show e)   => Show (EXCEPT e) where
  show (THROW e)    =  "Throw, " ++ show e
  show (MARK ops e) =  "(Mark - " ++ show ops ++ "), " ++ show e
  show (UNMARK e)   =  "Unmark, " ++ show e
  
--- SHOWING STACKS ---

instance Show Stack where
  show (In x) = show x
  
instance (Show e) => Show (Integ e) where
  show (VAL n e) = "(Val " ++ show n ++ "), " ++ show e
  
instance (Show e) => Show (Handler e) where
  show (HAND ops e) = "Handle: [" ++ show ops ++ "], " ++ show e
  
--- SHOWING EMPTY ---
  
instance (Show e) => Show (EMPTY e) where
  show NULL     = "End"

{-==== FIN. ====-}
