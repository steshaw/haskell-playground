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

  ghci lecture1.lhs

at the command line. 

----------------------------------------------------------------------

Ignore the following: needed for technical reasons.

> {-# OPTIONS -XStandaloneDeriving -XFlexibleContexts -XUndecidableInstances #-}
> import Prelude hiding (sum,Functor,fmap,length)

----------------------------------------------------------------------

Lecture 1: Folds and Unfolds

In these lectures (and especially in these executable scripts), we'll
use Haskell syntax, but we will interpret it in SET rather than in CPO
- all functions will be total, and we will distinguish data from
codata.

Let's start with (finite) lists of integers:

> data IntList = NilI | ConsI Int IntList

> sum :: IntList -> Int
> sum NilI = 0
> sum (ConsI i x) = i + sum x

Here is an example list:

> intlist :: IntList
> intlist = ConsI 3 (ConsI 4 (ConsI 5 NilI))

You can print it out at the command line, and compute its sum:

*Main> intlist
ConsI 3 (ConsI 4 (ConsI 5 NilI))
*Main> sum intlist
12

----------------------------------------------------------------------

Many functions share sum's pattern of computation - two cases, for
empty and non-empty lists, and in the non-empty case the tail is used
only in a recursive call. Here are two such functions:

> prod :: IntList -> Int
> prod NilI = 1
> prod (ConsI i x) = i * prod x

> allEven :: IntList -> Bool
> allEven NilI = True
> allEven (ConsI i x) = even i && allEven x

Rather than writing the pattern over and over again, we should
abstract it, as a higher-order function.

> foldIntList :: b -> (Int -> b -> b) -> IntList -> b
> foldIntList e f NilI = e
> foldIntList e f (ConsI i x) = f i (foldIntList e f x)

This really is an abstraction of the three functions, because we can
retrieve then by instantiating the pattern:

> sum' = foldIntList 0 (+)
> prod' = foldIntList 1 (*)
> allEven' = foldIntList True ((&&) . even)

----------------------------------------------------------------------

Here is another recursive datatype - binary trees, with integers at
internal nodes:

> data IntTree = EmptyI | NodeI Int IntTree IntTree

They too have a fold pattern of computation:

> foldIntTree :: b -> (Int -> b -> b -> b) -> IntTree -> b
> foldIntTree e f EmptyI = e
> foldIntTree e f (NodeI i x y) = f i (foldIntTree e f x) (foldIntTree e f y)

----------------------------------------------------------------------

We could do this every time we think of a new recursive datatype; but
it's tedious to write the "obvious" fold pattern every time - we
should find a way of capturing that pattern.

The trick is to separate the recursion from the shape of the data
structure. Given a shape parameter f, the following definition
captures recursive datatypes of shape f:

> data Mu1 f = In1 (f (Mu1 f))

Here is a suitable value for the shape parameter, expressing the shape
of integer lists:

> data IntListF a = NilIF | ConsIF Int a

The "a" type parameter is for the type of recursive occurrences -
here, for what goes in the tail of a list. Note that IntListF is not a
recursive type; it declares only one level of the shape of an integer
list. To recover the recursive datatype, we instantiate Mu1 to the
shape IntListF.

> type IntList' = Mu1 IntListF

Here is the same example list as before, but in the new style.

> intlist' :: IntList'
> intlist' = In1 (ConsIF 3 (In1 (ConsIF 4 (In1 (ConsIF 5 (In1 NilIF))))))

----------------------------------------------------------------------

Not all type operations "f" are suitable as parameters to
Mu1. Moreover, for those that are, we have to declare "how to find the
recursive positions". We solve both problems at once, by declaring a
so-called "type constructor class". Only type operations in this class
are suitable, and to be in this class they have to support the
operation fmap.

> class Functor f where
>   fmap :: (a -> b) -> f a -> f b

IntListF is a member of this class:

> instance Functor IntListF where
>   fmap f NilIF = NilIF
>   fmap f (ConsIF i a) = ConsIF i (f a)

Instances of Functor should satisfy the following two laws:

  fmap id = id
  fmap (f . g) = fmap f . fmap g

You might try proving that the definition of fmap for IntListF does
indeed satisfy them. It's not hard, because IntListF is not recursive.

----------------------------------------------------------------------

A beneficial side-effect of separating out the recursion in the
datatype from the shape, is that a program that depends only on the
recursive nature of a datatype and can be parametrised by the shape
can be written once and for all, as a datatype-generic function. One
such example is fold.

> fold1 :: Functor f => (f b -> b) -> Mu1 f -> b
> fold1 phi (In1 x) = phi (fmap (fold1 phi) x)

This subsumes the earlier definition of foldIntList:

> foldIntList' :: b -> (Int -> b -> b) -> IntList' -> b
> foldIntList' e f = fold1 (phi e f) where
>   phi e f NilIF = e
>   phi e f (ConsIF i x) = f i x

----------------------------------------------------------------------

This is all very well, but it doesn't capture the essence of container
datatypes: the "Int" in IntListF is hard-wired. We want something
analogous to the polymorphic datatype

  data List a = Nil | Cons a (List a)

We achieve this by abstracting over the element type as well as over
the type of the recursive positions.

> class Bifunctor f where
>   bimap :: (a->c) -> (b->d) -> f a b -> f c d

As before, two laws should be satisfied:

  bimap id id = id
  bimap f g . bimap h j = bimap (f . h) (g . j)

Now a generic polymorphic datatype is given by

> data Mu f a = In { in_ :: f a (Mu f a) }

(This is a use of Haskell's record syntax. It is equivalent to
defining In as a constructor, as before, and separately defining
in_ as its inverse:

  data Mu f a = In (f a (Mu f a))
  in_ :: Mu f a -> f a (Mu f a)
  in_ (In x) = x

Defining both at once is a bit more elegant, in particular making
clear that In and in_ are each other's inverses.)

A polymorphic version of fold is nearly as before:

> fold :: Bifunctor f => (f a b -> b) -> Mu f a -> b
> fold phi (In x) = phi (bimap id (fold phi) x)

----------------------------------------------------------------------

For example, polymorphic lists have the following shape:

> data ListF a b = NilF | ConsF a b

That is, either empty, or non-empty with one element and one child. Of
course, this is a suitable bifunctor:

> instance Bifunctor ListF where
>   bimap f g NilF = NilF
>   bimap f g (ConsF a b) = ConsF (f a) (g b)

(Check that the laws are satisfied!)

Now we can define lists by specialising the datatype-generic recursive
pattern to this shape:

> type List a = Mu ListF a

and we can have lists of booleans, as well as lists of integers:

> boollist :: List Bool
> boollist = In (ConsF True (In (ConsF False (In NilF))))

This represents the list [True,False] of length 2. We can define
length as an instance of the polymorphic datatype-generic fold:

> length :: List a -> Int
> length = fold phi where
>   phi NilF = 0
>   phi (ConsF a b) = 1+b

*Main> length boollist
2

----------------------------------------------------------------------

More interestingly, we can now give a witness to these recursive
datatypes being container types, by defining a version of map for
them. Note that even though the shape (like ListF) is a two-parameter
operation on types, the datatype of this shape (like List) is a
one-parameter operation; so we get an instance of Functor.

> instance Bifunctor f => Functor (Mu f) where
>   fmap f (In x) = In (bimap f (fmap f) x)

In fact, this definition is an instance of fold; we could just as well
have defined

  instance Bifunctor f => Functor (Mu f) where
    fmap f = fold (In . bimap f id)

----------------------------------------------------------------------

Rather crucially, the definition of fold

  fold phi (In x) = phi (bimap id (fold phi) x)

is also an equation about fold. This is for all x; we can avoid that
slight awkwardness by writing it in point-free style:

  fold phi . In = phi . bimap id (fold phi)

or equivalently, by exploiting the isomorphism between In and in_,

  fold phi = phi . bimap id (fold phi) . in_

More than that, read as an equation in an unknown h,

  h . In = phi . bimap id h

it has a unique solution, namely fold phi:

  (h = fold phi) <=> (h . In = phi . bimap id h)

This is called the *universal property* of fold. We will probably come
back to this, during the rest of the course.

----------------------------------------------------------------------

This all dualises elegantly, to codatatypes. Operationally, these
consist of both finite and infinite data structures, whereas the
"ordinary" datatypes we have been discussing so far consist of just
the finite structures. In a certain sense, ordinary datatypes are
least fixpoints of equations on types (which you might think of as
sets of values), whereas codatatypes are greatest fixpoints. This
motivates the choices of name, Mu for least solution and Nu for
greatest:

> data Nu f a = Out_ { out :: f a (Nu f a) }

In the lectures, we wrote this using a keyword "codata". There is no
such facility in Haskell, because Haskell doesn't distinguish between
data and codata. (But total functional programming languages, like
Charity and Agda, do make this distinction.)

The natural pattern of computation on codata is to generate it, rather
than to consume it. For that, we use unfold:

> unfold :: Bifunctor f => (b -> f a b) -> b -> Nu f a
> unfold phi = Out_ . bimap id (unfold phi) . phi

which again has a universal property:

  (h = unfold phi) <=> (out . h = bimap id h . phi)

The codatatypes are also container types, as we can show by providing
a Functor instance for them. Naturally, the definition of fmap arises
as an unfold:

> instance Bifunctor f => Functor (Nu f) where
>   fmap f = unfold (bimap f id . out)

For example, colists are the codatatype of list shape:

> type Colist a = Nu ListF a

and the range function generates a (finite or infinite) colist of
integers from a pair of integers:

> range :: (Int,Int) -> Colist Int
> range = unfold next where
>   next (m,n) = if m==n then NilF else ConsF m (m+1,n)

*Main> range (1,3)
Out_ {out = ConsF 1 (Out_ {out = ConsF 2 (Out_ {out = NilF})})}
*Main> range (1,0)
Out_ {out = ConsF 1 (Out_ {out = ConsF 2 (Out_ {out = ConsF 3...

----------------------------------------------------------------------

You can also ignore the following; they're just to allow you to print
out terms in the read-eval-print loop.

> deriving instance Show IntList
> deriving instance Show (f (Mu1 f)) => Show (Mu1 f)
> deriving instance Show a => Show (IntListF a)
> deriving instance Show (f a (Mu f a)) => Show (Mu f a)
> deriving instance (Show a, Show b) => Show (ListF a b)
> deriving instance Show (f a (Nu f a)) => Show (Nu f a)
