PATTERNS IN FUNCTIONAL PROGRAMMING
Lecture Course at NII, Feb-Mar 2013
Jeremy Gibbons

Lecture 1: Folds and Unfolds
Lecture 2: Sorting Algorithms
Lecture 3: Generic Programming
Lecture 4: Accumulations
Lecture 5: Monads
Lecture 6: Distributivity
Lecture 7: Queries and Comprehensions
Lecture 8: Idioms

If you don't already have Haskell installed, I recommend that you
choose the Haskell Platform, an all-in-one download:

  http://www.haskell.org/platform/

Once you have installed Haskell, you can load and experiment with this
script by typing

  ghci lecture5.lhs

at the command line. 

----------------------------------------------------------------------

> import System.Random (getStdRandom, randomR)

----------------------------------------------------------------------

Lecture 5: Monads

Monads form an abstract datatype of computations. As well as yielding
a value, computations in general may have effects: I/O, exceptions,
mutable state, etc. Monads are a mechanism for cleanly incorporating
such impure features in a pure setting. We'll explore this mechanism
by investigating extensions to an evaluator for a simple language of
expressions:

> data Term = Con Integer | Div Term Term 
>   deriving Show

> good, bad :: Term
> good = Div (Con 7) (Div (Con 4) (Con 2))
> bad  = Div (Con 7) (Div (Con 2) (Con 4))

Here's the evaluator:

> eval :: Term -> Integer
> eval (Con u)   = u
> eval (Div x y) = eval x `div` eval y

For example,

  *Main> eval good
  3

This lecture is based on Dan Piponi's blog post "You Could Have
Invented Monads! (And Maybe You Already Have.)":

  http://blog.sigfpe.com/2006/08/you-could-have-invented-monads-and.html

----------------------------------------------------------------------

Evidently, evaluation may fail, because of division by zero:

  *Main> eval bad
  *** Exception: divide by zero

It's a bit awkward that eval is a partial function; let's handle the
exceptional behaviour instead. We introduce a datatype of results with
possible exceptions (analogous to the standard datatype Maybe a in
Haskell, or more precisely, Either String a):

> data Exc a = Raise Exception | Result a deriving Show
> type Exception = String

And here is an extended evaluator that correctly handles division by zero:

> evalE :: Term -> Exc Integer
> evalE (Con u) = Result u
> evalE (Div x y) = 
>   case evalE x of
>     Raise e -> Raise e
>     Result u -> case evalE y of
>       Raise e -> Raise e
>       Result v -> 
>         if v==0 then Raise "Division by zero" 
>                 else Result (u `div` v)

Now there are no runtime errors, only values:

  *Main> evalE good
  Result 3
  *Main> evalE bad
  Raise "Division by zero"

----------------------------------------------------------------------

We could instrument the evaluator to count evaluation steps. The
natural way to do this in a pure functional language is to pass the
current number of steps into each call of the evaluator, and to get an
updated number of steps out alongside the result. More generally, we
might want to pass other types of "state" around:

> newtype Counter a = C (State -> (a, State))
> run :: Counter a -> State -> (a,State)
> run (C f) = f

(This is a simplified version of the State monad in Haskell.)

Here's an extended evaluator that counts evaluation steps (one for
each constant and for each division):

> type State = Int

> evalC :: Term -> Counter Integer
> evalC (Con u)   = C (\ n -> (u, n+1))
> evalC (Div x y) = C (\ n -> 
>                     let (u, n')  = run (evalC x) (n+1)
>                         (v, n'') = run (evalC y) n'
>                     in (u `div` v, n''))

Now evaluating an expression yields a result of type Counter Integer,
which is a function (wrapped up in the data constructor C). We can't
print it, but we can apply it to an initial state, say zero, and print
the result:

  *Main> run (evalC good) 0
  (3,5)

So the value of the expression is 3, and it took 5 steps to compute
(three Cons and two Divs).

----------------------------------------------------------------------

We might also want to trace the evaluation steps, collecting a "log
file" as evaluation proceeds. We could do this using mutable state,
like we did for the counter, but that's more powerful than necessary,
because presumably no subsequent computation ever depends on what has
been written to the log. What we want is like the state monad, but
without the input state - ie just a pair consisting of the log and the
result:

> type Output = String
> newtype Trace a = T (Output, a) 

(This is equivalent to the Writer monad in Haskell. In this case, the
log file is a String, but in fact any monoid would do; so we could
have done the counter example by logging integers.)

> evalT :: Term -> Trace Integer
> evalT (Con u)   = T (line (Con u) u, u)
> evalT (Div x y) = let
>                     T (s,u)  = evalT x
>                     T (s',v) = evalT y
>                     p = u `div` v
>                   in T (s ++ s' ++ line (Div x y) p, p)

> line :: Term -> Integer -> Output
> line t n = "  " ++ show t ++ " yields " ++ show n ++ "\n"

For example,

  *Main> evalT good
  T ("  Con 7 yields 7\n  Con 4 yields 4\n  Con 2 yields 2\n  Div (Con 4) (Con 2) yields 2\n  Div (Con 7) (Div (Con 4) (Con 2)) yields 3\n",3)

The log file is a string with embedded newlines, which we can print
out more nicsely:

  *Main> let T (s,a) = evalT good in putStr s >> putStrLn ("Result is " ++ show a) 
    Con 7 yields 7
    Con 4 yields 4
    Con 2 yields 2
    Div (Con 4) (Con 2) yields 2
    Div (Con 7) (Div (Con 4) (Con 2)) yields 3
  Result is 3

----------------------------------------------------------------------

None of these extensions is difficult.  But each is rather awkward,
and obscures the previously clear structure.  How can we simplify the
presentation?  What do they have in common?

In all cases, there are ways of embedding "pure" computations, and of
sequencing computations. For type constructor "m", these are

  lift :: (a -> b) -> (a -> m b)
  comp :: (b -> m c) -> (a -> m b) -> (a -> m c)

There are also effect-specific operations for each m.

For exceptions, we have

> liftE :: (a -> b) -> (a -> Exc b)
> liftE f = Result . f

> compE :: (b -> Exc c) -> (a -> Exc b) -> (a -> Exc c)
> compE f g a = case g a of
>   Raise e -> Raise e
>   Result b -> f b

There is also an effect-specific operation to throw an exception.

> throw :: Exception -> Exc e
> throw e = Raise e

For counters, we have

> liftC :: (a -> b) -> (a -> Counter b)
> liftC f a = C (\ n -> (f a, n))

> compC :: (b -> Counter c) -> (a -> Counter b) -> (a -> Counter c)
> compC f g a = C (\ n -> let (b,n') = run (g a) n 
>                         in run (f b) n')

and the effect-specific behaviour is to increment the count:

> tick :: Counter ()
> tick = C (\ n -> ((),n+1))

For tracing, we have

> liftT :: (a -> b) -> (a -> Trace b)
> liftT f a = T ("", f a)

> compT :: (b -> Trace c) -> (a -> Trace b) -> (a -> Trace c)
> compT f g a = let T (s,b) = g a 
>                   T (s',c) = f b 
>               in T (s++s',c)

and the effect-specific behaviour is to log some output:

> trace :: String -> Trace ()
> trace s = T (s, ())

----------------------------------------------------------------------

Haskell actually chooses a different model of lifting:

  return :: a -> m a

The two models are equivalent - you can define each in terms of the other:

  lift f = return . f
  return = lift id

Similarly, it has a different model for composition:

  (>>=) :: m a -> (a -> m b) -> m b

and again it's equivalent:

  comp f g a = g a >>= f
  ma >>= f = comp f id ma

These are the methods of a type class:

  class Monad m where
    return :: a -> m a
    (>>=) :: m a -> (a -> m b) -> m b

Technically, there are some laws that should be satisfied.
These are clearest specified in terms of "comp":

  f `comp` return = f
  return `comp` f = f
  f `comp` (g `comp` h) = (f `comp` g) `comp` h

(so monads are intimately related to monoids). As an exercise, you
might want to work out the equivalent statement of the laws in terms
of return and >>=.

Exceptions instantiate the class:

> instance Monad Exc where
>   return = liftE id
>   ma >>= f = compE f id ma

That is,

  return a = Result a
  Raise e >>= f = Raise e
  Result a >>= f = f a

Counters instantiate the class:

> instance Monad Counter where
>   return = liftC id
>   ma >>= f = compC f id ma

That is,

  return a = C (\ n -> (a,n))
  ma >>= f = C (\ n -> let (a,n') = run ma n in run (f a) n')

And tracing instantiates the class:

> instance Monad Trace where
>   return = liftT id
>   ma >>= f = compT f id ma

That is,

  return a = T ("", a)
  T (s,a) >>= f = let T (s',b) = f a in T (s++s', b)

----------------------------------------------------------------------

We can now rewrite our original evaluator, but monadically - ie using
return and >>=.

> evalM :: Monad m => Term -> m Integer
> evalM (Con u)   = return u
> evalM (Div x y) = evalM x >>= \ u ->
>                      evalM y >>= \ v ->
>                        return (u `div` v) 

It is still a pure function (none of the additional effects are used),
but now it is written in the monadic style; this makes it much easier
to extend.

Then the exceptional evaluator can be written monadically, making only
a small change to the basic program - to throw an exception if the
divisor is zero:

> evalE2 :: Term -> Exc Integer
> evalE2 (Con u)   = return u
> evalE2 (Div x y) = evalE2 x >>= \ u ->
>                      evalE2 y >>= \ v ->
>                        if v==0 then throw "Division by zero"
>                                else return (u `div` v) 

Similarly, the counting evaluator executes a "tick" for each constructor:

> evalC2 :: Term -> Counter Integer
> evalC2 (Con u)   = tick >>= \ () -> 
>                      return u
> evalC2 (Div x y) = tick >>= \ () ->
>                      evalC2 x >>= \ u ->
>                        evalC2 y >>= \ v -> 
>                          return (u `div` v) 

And the tracing evaluator logs some output just before returning each result:

> evalT2 :: Term -> Trace Integer
> evalT2 (Con u)   = trace (line (Con u) u) >>= \ () ->
>                      return u
> evalT2 (Div x y) = evalT2 x >>= \ u ->
>                      evalT2 y >>= \ v ->
>                        let p = u `div` v in
>                          trace (line (Div x y) p) >>= \ () ->
>                            return p 

We had to change the structure of the original program; but now we
don't have to perform any other major changes to add each of the
special effects.

----------------------------------------------------------------------

Haskell provides "do notation" - a special syntactic sugar for monadic
expressions, like the three monadic evaluators above. This is inspired
by (in fact, a generalization of) list comprehensions. It is defined
by translation back into uses of >>=:

  do { m }           = m
  do { a <- m ; ms } = m >>= \ a -> do { ms }
  do {      m ; ms } = m >>= \ _ -> do { ms }

where a can appear free in ms.

Using the do notation, we can write the three monadic evaluators in
perhaps a more natural (in fact, an imperative) style:

> evalE3 :: Term -> Exc Integer
> evalE3 (Con u)   = do 
>                      return u
> evalE3 (Div x y) = do
>                      u <- evalE3 x
>                      v <- evalE3 y
>                      if v==0 then throw "Division by zero"
>                              else return (u `div` v) 

> evalC3 :: Term -> Counter Integer
> evalC3 (Con u)   = do
>                      tick 
>                      return u
> evalC3 (Div x y) = do
>                      tick
>                      u <- evalC3 x
>                      v <- evalC3 y
>                      return (u `div` v) 

> evalT3 :: Term -> Trace Integer
> evalT3 (Con u)   = do 
>                      trace (line (Con u) u)
>                      return u
> evalT3 (Div x y) = do
>                      u <- evalT3 x
>                      v <- evalT3 y
>                      let p = u `div` v
>                      trace (line (Div x y) p)
>                      return p 

----------------------------------------------------------------------

There's no magic to monads in general: all the monads above are just
plain (perhaps higher-order) data, implementing a particular
interface.

But there is one magic monad: the "IO" monad.  Its implementation is
abstract, hard-wired into the language implementation.

  data IO a = ...
  instance Monad IO where ...

(Technically, the representation is again just as plain data. However,
the runtime system treats this specific datatype differently. In GHCi,
when you evaluate any expression at the read-eval-print prompt, it
gets printed out; but if that expression is of type IO a, instead of
being printed it is interpreted as describing an effectful
computation, and this computation is executed. Similarly, the "main"
method of a compiled program should have IO type; running the program
amounts to evaluating the data structure the program describes, then
interpreting this data structure as a further computation to be
executed.)

Some IO-specific operations provided in the Haskell libraries are for
character I/O:

  putChar :: Char -> IO ()
  getChar :: IO Char

file handling:

  type FilePath = String
  writeFile :: FilePath -> String -> IO ()
  readFile :: FilePath -> IO String

and random number generation:

  data StdGen = ...       -- standard random generator
  class Random where ...  -- randomly generatable
  randomR :: Random a => (a,a) -> StdGen -> (a,StdGen)
  getStdRandom :: (StdGen -> (a,StdGen)) -> IO a

among many others.

These are ordinary Haskell functions, and can be composed with other
Haskell functions in the usual way. For example, the libraries also
provide functions for string output and input, but these can be
defined in terms of corresponding functions for characters:

  putStr, putStrLn :: String -> IO ()
  putStr ""    = do { return () }
  putStr (c:s) = do { putChar c ; putStr s }
  putStrLn s   = do { putStr s ; putChar '\n' }

  getLine :: IO String
  getLine = do 
    c <- getChar
    if c=='\n' then return "" else do
      s <- getLine
      return (c:s)

Similarly, if you can read and write files, then you can assemble a
simple file processor:

> processFile :: FilePath -> FilePath -> (String->String) -> IO ()
> processFile inFile outFile f = do
>   s <- readFile inFile
>   let s' = f s
>   writeFile outFile s'

And if you can generate random numbers, you can simulate the roll of
three dice:

> rollDice :: IO Int
> rollDice = getStdRandom (randomR (1,6))

> rollThrice :: IO Int
> rollThrice = do 
>   x <- rollDice
>   y <- rollDice
>   z <- rollDice
>   return (x+y+z)

----------------------------------------------------------------------

