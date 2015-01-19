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

  ghci lecture4.lhs

at the command line. 

----------------------------------------------------------------------

> {-# OPTIONS -XStandaloneDeriving -XFlexibleContexts -XUndecidableInstances -XTypeFamilies #-}

We'll provide our own (equivalent) definitions of some prelude functions:

> import Prelude hiding (foldr, foldl, scanr, scanl, sum)

Here are some definitions from Lecture 1 that we'll reuse.

> class Bifunctor f where
>   bimap :: (a->c) -> (b->d) -> f a b -> f c d

> data Mu f a = In { in_ :: f a (Mu f a) }
> deriving instance Show (f a (Mu f a)) => Show (Mu f a)

> fold :: Bifunctor f => (f a b -> b) -> Mu f a -> b
> fold phi = phi . bimap id (fold phi) . in_

> instance Bifunctor f => Functor (Mu f) where
>   fmap f = fold (In . bimap f id)

We'll also need ad-hoc datatype genericity, so we need to define a
universe of codes for bifunctors. The first two select each argument
of the bifunctor; we will use these arguments to represent elements
and children in recursive datastructures, respectively.

> newtype El a b = El { el_ :: a }
>   deriving Show
> newtype Ch a b = Ch { ch_ :: b }
>   deriving Show

The remainder represent constant functors, sum, product, and composition:

> newtype K c a b = K c
>   deriving Show
> data Sum p q a b = InL { inl_ :: p a b } | InR { inr_ :: q a b }
>   deriving Show
> data Prod p q a b = p a b :**: q a b
>   deriving Show

They're all bifunctors, of course:

> instance Bifunctor El where
>   bimap f g (El a) = El (f a)
> instance Bifunctor Ch where
>   bimap f g (Ch b) = Ch (g b)
> instance Bifunctor (K a) where
>   bimap f g (K c) = K c
> instance (Bifunctor p, Bifunctor q) => Bifunctor (Sum p q) where
>   bimap f g (InL x) = InL (bimap f g x)
>   bimap f g (InR y) = InR (bimap f g y)
> instance (Bifunctor p, Bifunctor q) => Bifunctor (Prod p q) where
>   bimap f g (x :**: y) = bimap f g x :**: bimap f g y

Here's the empty datatype - useful for some constructions:

> data Zero

There are no constructors for Zero, so no (proper) values of that
type; therefore the show function can never be called,

> instance Show Zero where 
>   show z = error "Zero"

and if you were somehow to be able to come up a value of type zero,
you deserve to be able to turn it into anything you want:

> magic :: Zero -> x
> magic z = seq z (error "cannot be called")

For example, here are externally labelled binary trees:

> type BtreeF = Sum El (Prod Ch Ch)
> type Btree a = Mu BtreeF a

For convenience, here are some "constructors":

> tip :: a -> Btree a
> tip a = In (InL (El a))
> bin :: Btree a -> Btree a -> Btree a
> bin t u = In (InR (Ch t :**: Ch u))

an example tree:

> t :: Btree Int
> t = bin (tip 2) (bin (tip 4) (tip 5))

and an example fold over these trees:

> add :: BtreeF Int Int -> Int
> add (InL (El n)) = n
> add (InR (Ch m :**: Ch n)) = m+n
> sum :: Btree Int -> Int
> sum = fold add

For testing purposes, we define a corresponding Haskell algebraic
datatype, and conversion to it:

> data Btree' a = Tip a | Bin (Btree' a) (Btree' a) deriving Show
> fromBtree :: Btree a -> Btree' a
> fromBtree = fold phi where
>   phi (InL (El a)) = Tip a
>   phi (InR (Ch t :**: Ch u)) = Bin t u

So, for example,

  *Main> t
  In {in_ = InR (Ch {ch_ = In {in_ = InL (El {el_ = 2})}} :**: Ch {ch_ = In {in_ = InR (Ch {ch_ = In {in_ = InL (El {el_ = 4})}} :**: Ch {ch_ = In {in_ = InL (El {el_ = 5})}})}})}
  *Main> fromBtree t
  Bin (Tip 2) (Bin (Tip 4) (Tip 5))
  *Main> sum t
  11

----------------------------------------------------------------------

Lecture 4: Accumulations

Recall the standard definitions of tails, foldr and scanr from the
Haskell libraries:

> tails :: [a] -> [[a]]
> tails [] = [[]]
> tails x = x : tails (tail x)

> foldr :: (a->b->b) -> b -> [a] -> b
> foldr f e [] = e
> foldr f e (a:x) = f a (foldr f e x)

> scanr :: (a->b->b) -> b -> [a] -> [b]
> scanr f e [] = [e]
> scanr f e (a:x) = f a (head y) : y where y = scanr f e x

For example,

  *Main> scanr (+) 0 [1,2,3]
  [6,5,3,0]

Note the important "scan lemma"

  scanr f e = map (foldr f e) . tails

relating these three functions; this is very important in deriving
some efficient algorithms over lists, not least for the famous
"maximum segment sum" problem.

Dually, there are functions that work from the opposite end of the
list:

> inits :: [a] -> [[a]]
> inits [] = [[]]
> inits (a:x) = [] : map (a:) (inits x)

> foldl :: (b->a->b) -> b -> [a] -> b
> foldl f e [] = e
> foldl f e (a:x) = foldl f (f e a) x

> scanl :: (b->a->b) -> b -> [a] -> [b]
> scanl f e [] = [e]
> scanl f e (a:x) = e : scanl f (f e a) x

  *Main> scanl (+) 0 [1,2,3]
  [0,1,3,6]

Again, there is a scan lemma:

  scanl f e = map (foldl f e) . inits

This lecture is about generalizing these functions to arbitrary
datatypes. We will get datatype-generic accumulations, but one will be
parametrically datatype-generic and one ad-hoc datatype-generic.

----------------------------------------------------------------------

Generalizing tail segments and scanr is the easier task, because they
follow the structure of the datatype, whereas inits and scanl in some
sense go against the grain. You can read more about this construction
in the section "Tail segments, datatype-generically" my blog post on
Horner's Rule:

  http://patternsinfp.wordpress.com/2011/05/05/horners-rule/

(Ignore the following section on initial segments in that blog post,
because it gives a different generalization than the one we will study
here. We'll probably come back to that different generalization later.)

The trick is due to Richard Bird, Oege de Moor, and Paul Hoogendijk,
in their paper "Generic Functional Programming with Types and
Relations": define a "labelled variant" for any datatype, which has
(precisely) one label at each node.

> newtype Label f a b = L { l_ :: (a, f () b) }
> type Ltree = Mu (Label BtreeF)

> root :: Mu (Label f) a -> a
> root = fst . l_ . in_

> instance Bifunctor f => Bifunctor (Label f) where
>   bimap f g = L . cross f (bimap id g) . l_

Here,

> cross :: (a->c) -> (b->d) -> (a,b) -> (c,d)
> cross f g (x,y) = (f x, g y)

As with Btree, for testing purposes we introduce an algebraic datatype
corresponding to Ltree:

> data Ltree' a = Leaf a | Node a (Ltree' a) (Ltree' a) deriving Show

> fromLtree :: Ltree a -> Ltree' a
> fromLtree = fold phi where
>   phi (L (a, InL (El ()))) = Leaf a
>   phi (L (a, InR (Ch t :**: Ch u))) = Node a t u

----------------------------------------------------------------------

Then subtrees labels every node with the subtree of the input rooted
at that node. It yields a labelled data structure, of the same shape
as the input expect for having a label at each node. The root label of
the output is the whole of the input; and each child in the output is
generated from the corresponding child in the input.

> subtrees :: Bifunctor f => Mu f a -> Mu (Label f) (Mu f a)
> subtrees = fold (In . L . fork (In . bimap id root) (bimap bang id))

An upwards accumulation is like subtrees, except that it folds every
tree it generates. It does this as it goes, so it takes no longer to
compute than a mere fold of the input tree does.

> scanu :: Bifunctor f => (f a b -> b) -> Mu f a -> Mu (Label f) b
> scanu phi = fold (In . L . fork (phi . bimap id root) (bimap bang id))

Here, bang and fork are basic combinators for unit and product:

> bang :: a -> ()
> bang a = ()

> fork :: (c->a) -> (c->b) -> c -> (a,b)
> fork f g c = (f c, g c)

Note that we have the all-important scan lemma:

  scanu phi = fmap (fold phi) . subtrees

For example,

  *Main> fromLtree (fmap fromBtree (subtrees t))
  Node (Bin (Tip 2) (Bin (Tip 4) (Tip 5))) (Leaf (Tip 2)) (Node (Bin (Tip 4) (Tip 5)) (Leaf (Tip 4)) (Leaf (Tip 5)))

  *Main> fromLtree (scanu add t)
  Node 11 (Leaf 2) (Node 9 (Leaf 4) (Leaf 5))

  *Main> fromLtree (fmap (fold add) (subtrees t))
  Node 11 (Leaf 2) (Node 9 (Leaf 4) (Leaf 5))

exploiting the conversion functions fromLtree and fromBtree (it's
entertaining to see how much output you get if you don't use these).
The scan clearly includes the values 2, 4, 5, 9, and 11, which are the
partial sums arising when computing sum t.

You might want to define a datatype of lists using Mu, as in lecture
1, then perform a scanu on a list and verify that it has the same
behaviour as scanr.

As another exercise, try writing subtrees and scanu as paramorphisms.

----------------------------------------------------------------------

Generalizing inits and scand is more difficult. The function inits
labels every node of a list with its list of predecessors, and the
generic version should label every node of a data structure with the
ancestors of that node; but the ancestors form a completely different
datatype, of linear shape whatever the branching structure of the
original. Similarly, scand will label every node of a data structure
with some function of its ancestors - not just any function, but some
kind of fold that will allow us to compute the whole scan in linear
time. You can read more about this part of the lecture in my blog post
on accumulations:

  http://patternsinfp.wordpress.com/2011/07/12/upwards-and-downwards-accumulations/

To capture ancestors, we need to make a detour through derivatives of
datatypes. The code here is based on Conor McBride's paper "Clowns to
the Left of Me, Jokers to the Right" (POPL 2008).

For our purposes here, we want derivatives in the second argument of a
bifunctor - ie for bifunctor f we want another bifunctor Delta2 f such
that Delta2 f a b is like f a b but with precisely one b missing:
Delta2 f a b is a one-b-hole context for f a b. We do that through a
type class Diff2 (of bifunctors differentiable in their second
argument), which has Delta2 as an associated type synonym:

> class Bifunctor f => Diff2 f where
>   type Delta2 f :: * -> * -> *

The class also has two methods, to consume and to produce holes:

>   positions :: f a b -> f a (b, Delta2 f a b)
>   plug2 :: (b, Delta2 f a b) -> f a b

The idea is that positions takes a complete piece of data, and labels
every b in it with the one-hole-context for which b completes the
original data structure; whereas plug2 takes a one-hole-context and a
value to fill that hole, and puts the latter in the former to make a
complete piece of data. The two are related by the following two laws
(which I think completely determine their implementations):

  bimap id fst (positions x) = x
  bimap id plug2 (positions x) = bimap id (const x) x

Informally, positions really annotates, so that discarding the
annotations is a left inverse; and plugging together each pair in the
output of positions produces many copies of the original data
structure.

Here are the instances for our universe of codes. 

The constant functor has no b-values in, so the derivative is the
empty type, positions has no effect, and you can never have a hole to
plug.

> instance Diff2 (K c) where
>   type Delta2 (K c) = K Zero
>   positions (K c) = K c
>   plug2 (b, K z) = magic z

Elements (the "a"s in f a b) also have no b-values, so have a similar instance:

> instance Diff2 El where
>   type Delta2 El = K Zero
>   positions (El a) = El a
>   plug2 (b, K z) = magic z

Children (the "b" in f a b) have precisely one b-value, so what's left
when this is deleted is the unit type:

> instance Diff2 Ch where
>   type Delta2 Ch = K ()
>   positions (Ch b) = Ch (b, K ())
>   plug2 (b, K ()) = Ch b

A value of a sum type is either of the left summand or of the right,
and in each case a one-b-hole context is a corresponding one-b-hole
context for that summand; so the two methods simply follow the
structure.

> instance (Diff2 p, Diff2 q) => Diff2 (Sum p q) where
>   type Delta2 (Sum p q) = Sum (Delta2 p) (Delta2 q)
>   positions (InL pxy) = InL (bimap id (cross id InL) (positions pxy))
>   positions (InR qxy) = InR (bimap id (cross id InR) (positions qxy))
>   plug2 (b, InL dp) = InL (plug2 (b, dp))
>   plug2 (b, InR dq) = InR (plug2 (b, dq))

A value of a product type is a pair, and a one-b-hole context for the
pair is either a one-b-hole context for the left half, together with
an intact right half, or an intact left half and a context for the
right half.

> instance (Diff2 p, Diff2 q) => Diff2 (Prod p q) where
>   type Delta2 (Prod p q) = Sum (Prod (Delta2 p) q) (Prod p (Delta2 q))
>   positions (x :**: y) = bimap id (cross id (InL . (:**: y))) (positions x) :**: 
>                          bimap id (cross id (InR . (x :**:))) (positions y)
>   plug2 (b, InL (dx :**: y)) = plug2 (b, dx) :**: y
>   plug2 (b, InR (x :**: dy)) = x :**: plug2 (b, dy)

For example, consider the type BtreeF Int Char, whose values have
either a single Int or a pair of Chars. The derivative in the second
argument Delta2 BtreeF Int Char of this type represents data
structures with one missing Char. Expanding the definitions from
the type class instances, we see that

  Delta2 BtreeF = Sum (K Zero) (Sum (Prod (K ()) Ch) (Prod Ch (K ())))

The left-hand variant of this sum is void: corresponding BtreeF values
have a single Int, and there is no way for such a value to be missing
a Char. The right-hand variant is itself a sum: corresponding BtreeF
values have two Chars, so there are two ways for such a value to be
missing a Char, and in each case what remains is the other Char.

Here is a piece of data in the right-hand variant of BtreeF Int Char:

> u :: BtreeF Int Char
> u = InR (Ch 'a' :**: Ch 'b')

Here are two one-Char-hole contexts for u, in each case having unit in
place of one of the Chars:

> v1, v2 :: Delta2 BtreeF Int Char
> v1 = InR (InL (K () :**: Ch 'b'))
> v2 = InR (InR (Ch 'a' :**: K ()))

If you plug the correct Char back into each context, you get the
original data back again:

> u1, u2 :: BtreeF Int Char
> u1 = plug2 ('a', v1)
> u2 = plug2 ('b', v2)

  *Main> u
  InR {inr_ = Ch {ch_ = 'a'} :**: Ch {ch_ = 'b'}}
  *Main> u1
  InR {inr_ = Ch {ch_ = 'a'} :**: Ch {ch_ = 'b'}}
  *Main> u2
  InR {inr_ = Ch {ch_ = 'a'} :**: Ch {ch_ = 'b'}}

----------------------------------------------------------------------

Incidentally, zippers are intimately connected with derivatives. A
zipper represents a data structure with a single subterm highlighted
as a "focus". Concretely, a zipper is a pair. The first component is
the subterm in focus. The second component is the remainder of the
data structure, expressed as a sequence of layers, like an onion,
innermost first; each layer is the one-hole context into which the
structure inside fits.

> type Zipper f a  = (Mu f a, [Delta2 f a (Mu f a)]) -- innermost is first

To reconstruct the complete data structure from the zipper, we plug
subterms into contexts, from the inside out:

> close :: Diff2 f => Zipper f a -> Mu f a
> close (x,ds) = foldl glue x ds where glue x d = In (plug2 (x, d))

For example, t1 is a little tree and tc2,tc3 two surrounding contexts:

> t1 :: Btree Int
> tc2, tc3 :: Delta2 BtreeF Int (Btree Int)
> t1 = tip 4
> tc2 = InR (InL (K () :**: Ch (tip 5)))
> tc3 = InR (InR (Ch (tip 2) :**: K ()))

If you fit these all together in the correct way, you can reconstruct
the tree t from above:

> t' :: Btree Int
> t' = close (t1, [ tc2, tc3 ])

  *Main> fromBtree t
  Bin (Tip 2) (Bin (Tip 4) (Tip 5))
  *Main> fromBtree t'
  Bin (Tip 2) (Bin (Tip 4) (Tip 5))

But, back to downwards accumulations...

----------------------------------------------------------------------

Now, paths are a simpler kind of zipper - without the subterm in focus
(so just a list), and without all the siblings either (so the second
argument is the unit type).

> type Path f a = [Delta2 f a ()] -- innermost is first

The function "paths" takes a data structure and labels every node with
the path from the root to that node:

> paths :: (Diff2 f, Bifunctor (Delta2 f)) => Mu f a -> Mu (Label f) (Path f a)

We define it using an accumulating parameter, the "path so far", which
is initially the empty list. The auxilliary function "paths'" is like
paths, but it takes the initial path as an additional argument; all of
the paths in the result will have this as a tail (remember that paths
are represented innermost first, so descendants of a common ancestor
will be labelled by paths with a common tail).

> paths t = paths' (t,[]) 
> paths' :: (Diff2 f, Bifunctor (Delta2 f)) => (Mu f a, Path f a) -> Mu (Label f) (Path f a)

The definition of paths' is a bit ugly, but not too difficult to
follow. From a data structure t we find all the children and their
contexts, using positions. For each context, we discard all the
siblings, then cons this layer onto the current path p; this gives the
inputs for a recursive call. We then construct a labelled data
structure by discarding the root labels and pairing with the incoming
path p.

> paths' (t, p) = In (L (p, 
>     bimap (const ()) (paths' . cross id ((:p) . bimap id (const ()))) 
>     (positions (in_ t))))

(Exercise: paths' isn't a fold. What pattern of recursion does it follow?)

A downwards accumulation is then a fold mapped over the paths:

  scand f e = fmap (foldr f e) . paths   -- scan lemma

But because we carefully arranged that the paths to children share a
subterm with the path to their parent, we can compute this
incrementally in linear time, assuming that f takes constant time:

> scand :: (Diff2 f, Bifunctor (Delta2 f)) => 
>          (Delta2 f a () -> b -> b) -> b -> Mu f a -> Mu (Label f) b
> scand f e t = scand' f (t, e) where
>   scand' f (t, b) = In (L (b, 
>     bimap (const ()) (scand' f . cross id ((`f` b) . bimap id (const ()))) 
>     (positions (in_ t))))

For example, we can produce a little guidebook for a tree, recording
in user-friendly format the path to each node in the tree. First we
need to describe each possible turn we might make:

> direction :: Show a => Delta2 BtreeF a () -> String
> direction (InL (K z)) = magic z
> direction (InR (InL (K () :**: Ch ()))) = "Left"
> direction (InR (InR (Ch () :**: K ()))) = "Right"

Then building the guidebook is a downwards accumulation:

> directions :: Show a => Btree a -> Ltree String
> directions = scand (\ x s -> direction x ++ ", " ++ s) "Stop"

  *Main> fromLtree (directions t)
  Node "Stop" (Leaf "Left, Stop") (Node "Right, Stop" (Leaf "Left, Right, Stop") (Leaf "Right, Right, Stop"))

----------------------------------------------------------------------
