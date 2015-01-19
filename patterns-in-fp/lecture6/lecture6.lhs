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

  ghci lecture6.lhs

at the command line. 

----------------------------------------------------------------------

We need to import quite a lot of library stuff:

> {-# OPTIONS -XStandaloneDeriving -XFlexibleContexts -XUndecidableInstances -XFlexibleInstances #-}

> import Prelude hiding (foldr)
> import Data.List (inits, tails)
> import Control.Monad (MonadPlus, mplus, mzero, liftM, join)
> import Control.Applicative (Applicative, pure, (<*>), WrappedMonad(..))
> import Data.Foldable (Foldable, foldr)

We will use origami operators from Lecture 1:

> class Bifunctor f where
>   bimap :: (a->c) -> (b->d) -> f a b -> f c d

> data Mu f a = In { in_ :: f a (Mu f a) }

> fold :: Bifunctor f => (f a b -> b) -> Mu f a -> b
> fold phi = phi . bimap id (fold phi) . in_

> instance Bifunctor f => Functor (Mu f) where
>   fmap f = fold (In . bimap f id)

For example, here are (homogeneous) binary trees:

> data F a b = LeafF a | NodeF a b b
> instance Bifunctor F where
>   bimap f g (LeafF a) = LeafF (f a)
>   bimap f g (NodeF a b b') = NodeF (f a) (g b) (g b')

> type T a = Mu F a

> add :: F Int Int -> Int
> add (LeafF n) = n
> add (NodeF m n p) = m+n+p

> t :: T Int
> t = In (NodeF 4 (In (LeafF (-2))) (In (NodeF (-3) (In (LeafF 4)) (In (LeafF (-5))))))

For ease of testing, we'll define a mapping to an ordinary algebraic datatype:

> data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving Show
> toTree :: T a -> Tree a
> toTree = fold phi where
>   phi (LeafF a) = Leaf a
>   phi (NodeF a t u) = Node a t u

For example:

  *Main> toTree t
  Node 4 (Leaf (-2)) (Node (-3) (Leaf 4) (Leaf (-5)))
  *Main> fold add t
  -2

We'll also use labelled datatypes and upwards accumulations from Lecture 4:

> newtype Label f a b = Lab { lab_ :: (a, f () b) } 

> instance Bifunctor f => Bifunctor (Label f) where
>   bimap f g (Lab (a, ts)) = Lab (f a, bimap id g ts)

> root :: Mu (Label f) a -> a
> root = fst . lab_ . in_

> subterms :: Bifunctor f => Mu f a -> Mu (Label f) (Mu f a)
> subterms = fold phi
>   where phi x = In (Lab (In (bimap id root x), bimap (const ()) id x))

> scan :: Bifunctor f => (f a b -> b) -> Mu f a -> Mu (Label f) b
> scan phi = fold psi
>   where psi x = In (Lab (phi (bimap id root x), bimap (const ()) id x))

with the all-important Scan Lemma

  fmap (fold phi) . subterms = scan phi

For ease of testing, we will introduce an algebraic datatype
corresponding to the labelled variant of Mu F.

> data Ltree a = LLeaf a | LNode a (Ltree a) (Ltree a) deriving Show
> toLtree :: Mu (Label F) a -> Ltree a
> toLtree = fold phi where
>   phi (Lab (a, LeafF ())) = LLeaf a
>   phi (Lab (a, NodeF () t u)) = LNode a t u

For example,

  *Main> toLtree (fmap toTree (subterms t))
  LNode (Node 4 (Leaf (-2)) (Node (-3) (Leaf 4) (Leaf (-5)))) (LLeaf (Leaf (-2))) (LNode (Node (-3) (Leaf 4) (Leaf (-5))) (LLeaf (Leaf 4)) (LLeaf (Leaf (-5))))

----------------------------------------------------------------------

Lecture 6: Distributivity

Much of program calculation - and especially calculations that aim to
improve efficiency - amounts to applications of
distributivity. MapReduce is one, as Zhenjiang Hu's papers on the
"generate, test, aggregate" paradigm demonstrate. Here's a simpler
one, based on Horner's Rule. This lecture is based on my blog posting
on Horner's Rule

  http://patternsinfp.wordpress.com/2011/05/05/horners-rule/

and on the subsequent paper "Maximum Segment Sum, Monadically"

  http://arxiv.org/abs/1109.0782

(If you do read them, note that the blog post is slightly wrong: it
declares that you can restrict attention to non-empty collections, but
I now don't believe this. The paper at the arXiv fixes the problem,
and also has lots of accompanying exercises.)

The "maximum segment sum" problem is to compute the maximum of the
sums of all the segments of a list

> mss :: [Integer] -> Integer
> mss = maximum . map sum . segs

where a segment is an initial part of a tail part:

> segs :: [a] -> [[a]]
> segs = concat . map inits . tails

For example, with input

> x :: [Integer]
> x = [3,-1,-4,1,-5,-9,2,6,-5,3,5,-8,9,7,-9,3]

we have

  *Main> mss x
  19

(the segment in question being [2,6,-5,3,5,-8,9,7]). 

The specification is already a program, but one that takes cubic
time. It's a standard calculation, given first by Richard Bird in his
"Theory of Lists" work, to deduce that mss = mss' where

> mss' :: [Integer] -> Integer
> mss' = maximum . scanr op 0 where op u z = 0 `max` (u + z)

which takes only linear time. The essence of the calculation is
Horner's Rule, which applies here because addition distributes over
maximum.

In this lecture, we'll study a datatype-generic version of the maximum
segment sum problem.

----------------------------------------------------------------------

We will make use of collection monads: in Haskell, these are
represented by the MonadPlus type class, which as well as the monad
methods has mzero (the "empty collection") and mplus (a binary "union"
of collections).

An algebra for a collection monad m is a pair (b,k) where b is a type
and k :: m b -> b that respects the collection monad structure. This
means it must respect the monad structure more generally:

  k . return = id
  k . join = k . liftM k

but also the monoidal extensions: there exists an e and op such that

  k mzero = e
  k (mplus x y) = op (k x) (k y)

Hence, such a k amounts to a "reduce", in the old Theory of Lists
terminology: it is determined by the e and op.

> reduce :: (Foldable m, MonadPlus m) => (b->b->b) -> b -> m b -> b
> reduce = foldr

We define a type synonym for the tuple of the reduce function k and
its determinants op and e (we will need to be able to extract the op
and e from the reducer, but also to fix the particular m):

> type Reducer m b = (b->b->b, b, m b -> b)

From the equations above one can deduce the e and op must obey at
least the laws that mzero and mplus do; for example, if mplus is
associative and mzero is its unit, then op too must be associative and
e its unit.

----------------------------------------------------------------------

We now need to make a detour, through "applicative functors" (or
"idioms") and "traversable datatypes", following the work of Conor
McBride and Ross Paterson. This is something that we'll cover in
Lecture 8; apologies for it being out of order.

A polymorphic datatype f is traversable if there is a way of visiting
its elements in order. The order is significant, because by "visiting"
we mean doing so with computational effects. For these, we'll use a
slight generalization of monads called "applicative functors" - but for
the time being, you can think of Applicative as being the same as
Monad. 

Similarly, a bifunctor is bitraversable if there is a way of visiting
both kinds of element, again in order. We'll only need this latter
class for this lecture.

> class Bifunctor f => Bitraversable f where
>   bitraverse :: Applicative m => (a -> m c) -> (b -> m d) -> f a b -> m (f c d)

Our shape functor F is bitraversable; roughly speaking, this specifies
the ordering on the three components of a NodeF.

> instance Bitraversable F where
>   bitraverse f g (LeafF a) = pure LeafF <*> f a
>   bitraverse f g (NodeF a t u) = pure NodeF <*> f a <*> g t <*> g u

And if f is bitraversable, then so is Lab f: this definition
stipulates that the root label is visited first.

> instance Bitraversable f => Bitraversable (Label f) where
>   bitraverse f g (Lab (a, ts)) = pure (curry Lab) <*> f a <*> bitraverse pure g ts

All this works for any monad, because every monad induces an
applicative functor. For example, we can use bitraverse to distribute
the shape functor over a monad - which is to say, given computations
to generate each of the b-values, they can be sequenced to together to
make one computation yielding (f a b)-values.

> distr :: (Bitraversable f, Monad m) => f a (m b) -> m (f a b)
> distr = unwrapMonad . bitraverse pure WrapMonad 

The point of generalizing to applicative functors is that it also
works for some functors that aren't monads.  Any constant functor
yielding a monoidal type is one such:

> newtype Collect m b a = Coll { coll_ :: m b }
> instance Functor (Collect m b) where
>   fmap f = Coll . coll_

In particular, any collection monad has monoidal structure - the mplus
and mzero of MonadPlus (the return and >>= of the monad aren't used
here):

> instance MonadPlus m => Applicative (Collect m b) where
>   pure a = Coll mzero
>   fs <*> xs = Coll (coll_ fs `mplus` coll_ xs)

Using this applicative functor, we can traverse any data structure of
bitraversable shape, collecting all the elements into any collection
structure:

> contents :: (Bitraversable f, MonadPlus m) => Mu f a -> m a
> contents = coll_ . fold (bitraverse (Coll . return) (fmap (const ())))

For example,

  *Main> contents t :: [Int]
  [4,-2,-3,4,-5]

----------------------------------------------------------------------

For a datatype-generic version of MSS, we need a datatype-generic
definition of segments, and hence of inits and tails.  We've already
seen how to generalize tails to subterms, when we looked at upwards
accumulations.

The generalization of an "initial segment" of a data structure is a
similar data structure, but where some subterms may have been
discarded. To capture this, we introduce a type of "pruning": a value
of type Mu (Pruning f) a is like a Mu f a, except that some subterms
may be replaced by Nothing.

> newtype Pruning f a b = Pru { pru_ :: Maybe (f a b) } deriving Show
> instance Bifunctor f => Bifunctor (Pruning f) where
>   bimap f g = Pru . fmap (bimap f g) . pru_

Again, for ease of testing, we introduce an algebraic datatype
corresponding to prunings of Mu F:

> data Ptree a = PEmpty | PLeaf a | PNode a (Ptree a) (Ptree a) deriving Show
> toPtree :: Mu (Pruning F) a -> Ptree a
> toPtree = fold phi where
>   phi (Pru Nothing) = PEmpty
>   phi (Pru (Just (LeafF a))) = PLeaf a
>   phi (Pru (Just (NodeF a t u))) = PNode a t u

Now, the function prune takes a data structure and returns the
collection of all possible ways of pruning it (or, if you prefer, it
non-deterministically picks a way of pruning it). It's a fold, the
body of which works as follows. Assuming that you've computed the
collection of possible prunings of each child, then the prunings of
the parent are either the completely empty tree, or a node in which
each child is pruned in some way.

> prune :: (Bitraversable f, MonadPlus m) => Mu f a -> m (Mu (Pruning f) a)
> prune = fold (liftM (In . Pru) . opt Nothing . liftM Just . distr)

Here, "opt" adds one more option to a collection of values:

> opt :: MonadPlus m => a -> m a -> m a
> opt a x = return a `mplus` x

Then the datatype-generic MSS problem can be specified as follows:

> gmss :: (Bitraversable f, MonadPlus m) => Reducer m b -> (f a b -> b) -> b -> Mu f a -> b
> gmss (op,e,k) f b = k . liftM (fold (maybe b f . pru_)) . gsegs

where

> gsegs :: (Bitraversable f, MonadPlus m) => Mu f a -> m (Mu (Pruning f) a)
> gsegs = join . liftM prune . contents . subterms

and we have

  *Main> gmss (max,minBound,maximum) add 0 t
  5

as you may check.

----------------------------------------------------------------------

Now, the appropriate notion of distributivity, as explained in the
paper and blog, is that

  k . liftM f . distr = f . bimap id k

and assuming this distributivity property one can prove a
datatype-generic version of Horner's Rule

  k . liftM (fold (maybe b f)) . prune = fold (op b . f)

from which the Scan Lemma entails that gmss can be rewritten as

> gmss' :: (Bitraversable f, MonadPlus m) => Reducer m b -> (f a b -> b) -> b -> Mu f a -> b
> gmss' (op,e,k) f b = k . contents . scan (op b . f)

taking only linear time (assuming that f takes constant time, and k
linear time).

  *Main> gmss' (max,minBound,maximum) add 0 t
  5

----------------------------------------------------------------------

