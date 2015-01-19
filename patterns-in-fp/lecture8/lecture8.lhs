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

  ghci lecture8.lhs

at the command line. 

----------------------------------------------------------------------

> {-# OPTIONS -XStandaloneDeriving -XFlexibleInstances -XMultiParamTypeClasses #-}

> import Data.Monoid
> import Control.Monad (liftM, (<=<))
> import Control.Monad.State (MonadState, get, put)

Many of the definitions in this file are provided in the Haskell
libraries, for example in Control.Applicative and
Data.Traversable. But in order to discuss them, and variations on
them, we will start from first principles. I prefer the name "idiom"
to "applicative functor", so I'll use that throughout.

Here's some Origami stuff, that we'll use briefly just to make a point:

> class Bifunctor f where
>   bimap :: (a->c) -> (b->d) -> f a b -> f c d

> data Mu f a = In { in_ :: f a (Mu f a) }

> fold :: Bifunctor f => (f a b -> b) -> Mu f a -> b
> fold phi = phi . bimap id (fold phi) . in_

> instance Bifunctor f => Functor (Mu f) where
>   fmap f = fold (In . bimap f id)

----------------------------------------------------------------------

Lecture 8: Idioms (there is no code from Lecture 7)

In this lecture we will conduct an exercise in reasoning about
effectful programs, concerning a relabelling function on trees. Here
is a datatype of trees:

> data Tree a = Tip a | Bin (Tree a) (Tree a) deriving Show

> instance Functor Tree where
>   fmap f (Tip a) = Tip (f a)
>   fmap f (Bin t u) = Bin (fmap f t) (fmap f u)

> t :: Tree Char
> t = Bin (Tip 'a') (Bin (Tip 'b') (Tip 'c'))

We will express relabelling as a stateful function, using the State
monad:

> newtype State s a = State { runState :: s -> (a,s) }

This is, of course, a monad:

> instance Monad (State s) where
>   return a  = State (\ s -> (a,s))
>   ma >>= k  = State (\ s -> let (a,s') = runState ma s in runState (k a) s')

with additional operations to read and write the state:

> instance MonadState s (State s) where
>   get     = State (\ s -> (s,s))
>   put s'  = State (\ s -> ((),s'))

As you may check, these additional operations satisfy the following
four laws:

  put s >> put s'             = put s'
  put s >> get                = put s >> return s
  get >>= put                 = return ()
  get >>= \ s -> get >>= k s  = get >>= \ s -> k s s

In our case, the state will be an infinite supply of fresh labels, and
labelling a tree means attaching one of these labels to each element:

> label :: Tree a -> State [b] (Tree (a,b))
> label (Tip a)    = do { (b:y) <- get ; put y ; return (Tip (a,b)) }
> label (Bin u v)  = do { u' <- label u ; v' <- label v ; return (Bin u' v') }

For example, here's what happens when you label t:

> showWithStream :: (Show a, Show b) => (a, [b]) -> String
> showWithStream (a, bs) = "(" ++ show a ++ ", [" ++ showSome (take 5 bs) ++ "...])"
>   where showSome = concat . map ((++",") . show)

  *Main> showWithStream (runState (label t) [1..])
  "(Bin (Tip ('a',1)) (Bin (Tip ('b',2)) (Tip ('c',3))), [4,5,6,7,8,...])"

The three elements of t have been labelled with the first three
elements of the stream, and the remaining stream elements are the
resulting value of the state.

We want to "prove this program correct". What do we mean by this? Let
us accept (by inspection, perhaps) that it is clear that the tree
returned has the same shape as the one being traversed, and moreover,
the original elements are preserved. Then the only question that
remains is about the labels used; if we extract these too from the
resulting tree:

> labels :: Tree (a,b) -> [b]
> labels (Tip (a,b))  = [b]
> labels (Bin u v)    = labels u ++ labels v

and attach them to the remaining supply of fresh labels, then we get
back the original supply of fresh labels:

  runState (label t) xs = (u,ys)  ==>  labels u ++ ys = xs

We could conduct this proof monadically, using the monad laws and the
four laws of get and put; indeed, you can find such a proof in my
paper "Just Do It" with Ralf Hinze (ICFP 2011). But in this lecture,
we'll take a different approach, using idioms instead of monads. It is
based on my paper "Be Kind, Rewind" with Richard Bird; it's currently
unpublished, but on my webpage.

----------------------------------------------------------------------

Like monads, idioms are a specialization of functors:

> class Functor m => Idiom m where
>   pure   :: a -> m a
>   (<*>)  :: m (a->b) -> m a -> m b

Here, "pure" lifts a value to a computation, and <*> acts as a kind of
function application, but applying an effectful function to an
effectful argument to get an effectful result. These two operations
should satisfy the following four laws:

  pure id <*> ma                 =  ma
  pure (.) <*> mf <*> mg <*> ma  =  mf <*> (mg <*> ma)        
  pure f <*> pure a              =  pure (f a)             
  mf <*> pure a                  =  pure (\f -> f a) <*> mf

Any monad induces an idiom, with the following pattern:

> instance Functor (State s) where
>   fmap = liftM

> instance Idiom (State s) where
>   pure a     = return a
>   mf <*> ma  = do { f <- mf; a <- ma; return (f a) }

That is, "pure" is just "return", and idiomatic application runs the
effectful function then the effectful argument to get an ordinary
function and argument. An alternative definition of idiomatic
application would be as follows:

  mf <*> ma  = do { a <- ma; f <- mf; return (f a) }

yielding the effects of the argument before those of the function;
unless the monad is commutative, these two are different. We will come
back later to this alternative definition.

Another case of an idiom arising from a monad uses the Reader monad
(p->) for some type p. Equivalently, you can think of the type p->a as
a data structure of fixed shape, where p is the type of positions. For
example, streams are isomorphic to (Nat->), and pairs to
(Bool->). Then pure builds a data structure where the elements at each
position are all equal, and <*> does a kind of "zip with apply".

> newtype Reader p a = R (p -> a)

> instance Functor (Reader r) where
>   fmap f (R g) = R (f . g)

> instance Idiom (Reader p) where
>   pure a = R (\ p -> a)
>   R fs <*> R as = R (\ p -> (fs p) (as p))

I find this a very helpful idiom for getting an intuition for the four laws.

One thing that makes idioms interesting is that there are more of them
than there are monads - that is, every monad is an idiom, but also
some functors which are not monads are idioms too. In particular, the
constant functor

> newtype Const b a = Const { unConst :: b }

(which has no elements) is an idiom, when the constant type is a monoid:

> instance Functor (Const b) where
>   fmap f = Const . unConst

> instance Monoid b => Idiom (Const b) where
>   pure a = Const mempty
>   Const x <*> Const y = Const (x `mappend` y)

This is sometimes called a "phantom idiom", because the a parameter in
Const b a is a phantom type: a value of type Const b a has no "a"
elements inside. We will use this idiom later.

----------------------------------------------------------------------

The definition of "label" we gave earlier uses explicit recursion;
what's the pattern of computation involved? It's a straightforward
traversal of the tree, collecting effects as we go. It can be defined
for many datatypes, not just binary trees, so let's identify a type
class:

> class Functor t => Traversable t where
>   traverse :: Idiom m => (a -> m b) -> t a -> m (t b)

(We'll return below to the question of what laws traverse should
satisfy.) Here's the instance for trees:

> instance Traversable Tree where
>   traverse f (Tip a)    = pure Tip <*> f a
>   traverse f (Bin u v)  = pure Bin <*> traverse f u <*> traverse f v

For example, if we define a "body" that labels a single element from a
stream of fresh labels,

> adorn :: a -> State [b] (a,b)
> adorn a = do { (b:y) <- get; put y; return (a,b) }

then labelling of a whole tree is just traversal with this body: 
label = label' where

> label' :: Tree a -> State [b] (Tree (a,b))
> label' = traverse adorn

If you recall, we identified a bifunctor version of Traversable in
Lecture 6:

> class Bifunctor f => Bitraversable f where
>   bitraverse :: Idiom m => (a -> m c) -> (b -> m d) -> f a b -> m (f c d)

Bitraversable shape functors induce traversable datatypes:

> instance Bitraversable f => Traversable (Mu f) where
>   traverse f = fold (fmap In . bitraverse f id)

Bitraversable functors support distribution of the shape over an idiom:

> distr :: (Bitraversable f, Idiom m) => f a (m b) -> m (f a b)
> distr = bitraverse pure id

And traversable functors allow us to collect their contents:

> contents :: Traversable f => f a -> [a]
> contents = unConst . traverse (\ a -> Const [a])

This is a datatype-generic version of the "labels" function we had earlier.

----------------------------------------------------------------------

One benefit of idioms over monads is that they have better
compositional properties. The identity functor is an idiom, of course,
as it is a monad:

> newtype I a = I { unI :: a }

> instance Functor I where
>   fmap f = I . f . unI

> instance Idiom I where
>   pure a       =  I a
>   I f <*> I a  =  I (f a)

More interestingly, the composition of two idioms is an idiom, whereas
monads are not closed under composition.

> data C m n a = C { unC :: m (n a) }

> instance (Functor m, Functor n) => Functor (C m n) where
>   fmap f = C . fmap (fmap f) . unC

> instance (Idiom m, Idiom n) => Idiom (C m n) where
>   pure a           =  C (pure (pure a))
>   C mnf <*> C mna  =  C (pure (<*>) <*> mnf <*> mna)

The latter gives a composition operator for idiomatic functions; but
unlike Kleisli composition for monadic functions, it isn't necessary
for the two idioms to be the same.

> (<@>) :: (Idiom m, Idiom n) => (b -> n c) -> (a -> m b) -> a -> C m n c
> g <@> f = C . fmap g . f

Two of the laws we require for traverse are that it should respect the
compositional structure of idioms:

  traverse (I . f)           = I . fmap f
  traverse (C . fmap g . f)  = C . fmap (traverse g) . traverse f

Using idiomatic function composition, the second law can be rephrased
as a fusion property, combining two traversals into one:

  traverse (g <@> f) = traverse g <@> traverse f

----------------------------------------------------------------------

A third law we require is that traverse should respect idiom
morphisms. An idiom morphism phi from idiom m to idiom n is a
polymorphic function phi :: m a -> n a such that

  phi (pure a)     = pure a
  phi (mf <*> ma)  = (phi mf) <*> (phi ma)

Given an idiom morphism phi, then we require

  phi . traverse f  =  traverse (phi . f)

In particular, pure . unI :: I a -> m a is an idiom morphism (as you
may check), and so we get as a corollary that traversal with pure is
the idiomatic identity function:

  traverse pure = pure

Also, when idiom m arises from a commutative monad, then join . unC ::
C m m a -> m a is an idiom morphism, so (writing "<=<" for Kleisli
composition), we get another fusion law as a corollary:

  traverse g <=< traverse f = traverse (g <=< f)

----------------------------------------------------------------------

Now, we can try using the laws for fusing traversals on our tree
relabelling problem. Let's express the the unlabelling function as
another instance of traversal:

> unlabel :: Tree (a,b) -> State [b] (Tree a)
> unlabel = traverse strip

> strip :: (a,b) -> State [b] a
> strip (a,b) = do { y <- get; put (b:y); return a }

We might expect that unlabelling is the idiomatic inverse of
labelling; that is, that

  unlabel <@> label = pure

Let's calculate:

     unlabel <@> label
  =    {- definitions -}
     traverse strip <@> traverse adorn
  =    {- traversal respects composition -}
     traverse (strip <@> adorn)
  =    {- suppose that strip <@> adorn = pure -}
     traverse pure
  =    {- purity law -}
     pure

To apply this, we have to establish:

  strip <@> adorn = pure

But sadly, this property does not hold - you can construct an ordinary
Haskell function test such that

  test (strip <@> adorn) /= test pure

So that didn't work. However, both strip and adorn use the same
monadic idiom, and they are inverses using Kleisli composition:

  strip <=< adorn = return

Now we can reason:

     traverse strip <=< traverse adorn
  =    {- traversal respects Kleisli composition -}
     traverse (strip <=< adorn)
  =    {- since strip <=< adorn = return -}
     traverse return
  =    {- purity law -}
     return

But this whole line of reasoning is inapplicable, because the State
monad is not commutative.

----------------------------------------------------------------------

Of course, neither of the above approaches can possibly work, because
both traversals are left-to-right: the inverse of a left-to-right
traversal must surely be a right-to-left traversal. Indeed, the
function unlabel above strips the labels off a labelled tree, but puts
them back onto the stream of fresh labels again *in reverse order*.

  *Main> showWithStream (runState ((unlabel <=< label) t) [1..])
  "(Bin (Tip 'a') (Bin (Tip 'b') (Tip 'c')), [3,2,1,4,5,...])"

Happily, each idiom m has a complementary backwards twin B m:

> newtype B m a = B { unB :: m a }

> instance Functor m => Functor (B m) where
>   fmap f = B . fmap f . unB

> instance Idiom m => Idiom (B m) where
>   pure a         =  B (pure a)
>   B mf <*> B ma  =  B (pure (flip ($)) <*> ma <*> mf)

(in contrast to the situation with monads).

Using this, we can define backwards traversal:

> treverse :: (Idiom m, Traversable t) => (a -> m b) -> t a -> m (t b)
> treverse f = unB . traverse (B . f)

For example,

  treverse f (Bin u v) = pure (flip Bin) <*> treverse f v <*> treverse f u

given our earlier definition of traverse for Tree; and in general,
treverse f is like traverse f, but visiting the elements in the
opposite order.

In particular, we can define:

> unlabel' :: Tree (a, b) -> State [b] (Tree a)
> unlabel' = treverse strip

(Note that unlabel' is not equal to unlabel, because it performs
effects in the opposite order.)

Of course, B :: m a -> B m a is a natural transformation between two
idioms m and B m.  But it is not an idiom morphism; there is in
general no relationship between B mf <*> B ma and B (mf <*> ma).

However, the composition B . B :: m a -> m a is an idiom morphism:
reversing the order of effects is an involution.  Hence, a dual
characterization of traverse in terms of treverse:

     traverse f
  =    {- B is an isomorphism -}
     unB . unB . B . B . traverse f
  =    {- B . B is an idiom morphism -}
     unB . unB . traverse (B . B . f)
  =    {- definition of treverse -}
     unB . treverse (B . f)

What about Kleisli compositions of traverse and treverse? Perhaps a
law like this should hold:

  treverse f <=< traverse g = traverse (f <=< g) -- invalid!

Such a hope is still forlorn: the effects of f happen in opposite
orders on the two sides; and besides, the property is suspiciously
asymmetric (why traverse on the rhs, not treverse?).

Both misgivings disappear when f <=< g = return: then

  traverse (f <=< g) = traverse return = return

So let's impose this as an additional law:

  f <=< g = return  ==>  treverse f <=< traverse g = return

Assuming this inverse traversal law, we can solve our relabelling problem:

     unlabel' <=< label
  =    {- definitions -}
     treverse strip <=< traverse adorn
  =    {- given that strip <=< adorn = return -}
     return

And indeed

  *Main> showWithStream (runState ((unlabel' <=< label) t) [1..])
  "(Bin (Tip 'a') (Bin (Tip 'b') (Tip 'c')), [1,2,3,4,5,...])"

----------------------------------------------------------------------

This inverse traversal law 

  f <=< g = return  ==>  treverse f <=< traverse g = return

is a bit mysterious. It does not seem to follow from other properties.
Nevertheless, I don't know of a traverse that respects idiom
composition and idiom morphisms but not this reversal property.  Is it
the consequence of some deeper structure?

Here's a modest proposal. Following the general principle that one
should state healthiness conditions on abstract operations as the
axioms of a type class, we should make treverse an additional method
of Traversable.  We can give treverse a default definition in terms of
traverse, so this is no burden on the programmer.  But if they are
defined independently, then a proof obligation ensues. So here's my
proposed type class definition:

  class Functor t => Traversable t where
    traverse, treverse :: Idiom m => (a -> m b) -> t a -> m (t b)
    treverse f = unB . traverse (B . f)

with laws

  traverse (I . f)           = I . fmap f
  traverse (C . fmap g . f)  = C . fmap (traverse g) . traverse f
  traverse (phi . f)         = phi . traverse f  -- for idiom morphism phi
  treverse f <=< traverse g  = return  <==  f <=< g = return

Incidentally, McBride and Paterson include an operation 

  dist :: (Idiom m, Traversable t) => t (m a) -> m (t a)

in the type class too. dist and traverse are interdefinable. Perhaps
there should be a backwards version of dist too?

----------------------------------------------------------------------


