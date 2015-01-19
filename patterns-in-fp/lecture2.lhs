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

A few dependencies:

> {-# OPTIONS -XStandaloneDeriving -XFlexibleContexts -XUndecidableInstances #-}

> import Data.List (unfoldr)

The following definitions are from Lecture 1; we will reuse them here.

> class Bifunctor f where
>   bimap :: (a->c) -> (b->d) -> f a b -> f c d

> data Mu f a = In { in_ :: f a (Mu f a) }

> fold :: Bifunctor f => (f a b -> b) -> Mu f a -> b
> fold phi (In x) = phi (bimap id (fold phi) x)

> data Nu f a = Out_ { out :: f a (Nu f a) }

> unfold :: Bifunctor f => (b -> f a b) -> b -> Nu f a
> unfold phi = Out_ . bimap id (unfold phi) . phi

> data ListF a b = NilF | ConsF a b

> instance Bifunctor ListF where
>   bimap f g NilF = NilF
>   bimap f g (ConsF a b) = ConsF (f a) (g b)

> type List a = Mu ListF a
> type Colist a = Nu ListF a

> deriving instance Show (f a (Mu f a)) => Show (Mu f a)
> deriving instance Show (f a (Nu f a)) => Show (Nu f a)
> deriving instance (Show a, Show b) => Show (ListF a b)

----------------------------------------------------------------------

Lecture 2: Sorting Algorithms

Sorting takes lists to lists. Clearly the input has to be a finite
list; you can't sort an infinite list. We have a choice about the
output, though. If the input is a finite list, then so will be the
output, so we could use data for the output too. However, we will use
codata for the output, because that will give us extra structure to
exploit. 

In this lecture, we will assume a totally ordered type K of keys. For
simplicity, in this script we will fix that type as integers:

> type K = Int

So we will be looking at sorting functions of type

  List K -> Colist K

Here's some input to play with, representing th list [3,1,4,1,5]:

> input :: List K
> input = In (ConsF 3 (In (ConsF 1 (In (ConsF 4 (In (ConsF 1 (In (ConsF 5 (In NilF))))))))))

And here's a more user-friendly way of printing colists:

> toList :: Colist a -> [a]
> toList = unfoldr next where
>   next (Out_ NilF) = Nothing
>   next (Out_ (ConsF a x)) = Just (a, x)

which you can use to try out any of the sorting functions described here:

*Main> toList (isort input)
[1,1,3,4,5]

----------------------------------------------------------------------

One obvious place to start is to exploit the data structure in the
input: ie to use a fold.

> isort :: List K -> Colist K
> isort = fold insert

Consideration of the type of fold tells us that

> insert :: ListF K (Colist K) -> Colist K

The obvious next step here is to exploit the structure in the output
(since there is not much further structure in the input to exploit):

> insert = unfold ins

Again, consideration of the types tells us that

> ins :: ListF K (Colist K) -> ListF K (ListF K (Colist K))

This type looks complicated, but it's mostly non-recursive. (We have
to unpack the input Colist one level, but not further.) Consideration
of the cases - Nil or Cons - and the suspicion that we ought to use
comparison somewhere suggests the following program:

> ins NilF = NilF
> ins (ConsF a (Out_ NilF)) = ConsF a NilF
> ins (ConsF a (Out_ (ConsF b x)))
>   | a <= b = ConsF a (ConsF b x)
>   | a > b  = ConsF b (ConsF a x)

I haven't proved to you that this does indeed sort, but it is clear
that it follows structural principles; you can test that it works, or
trust me. So one way of sorting is as a fold, whose body is an unfold.

----------------------------------------------------------------------

Conversely, we might start by exploiting the structure of the output,
leading to an unfold:

> bsort :: List K -> Colist K
> bsort = unfold bubble

Consideration of the types gives us

> bubble :: List K -> ListF K (List K)

from which it is reasonable to try making bubble a fold:

> bubble = fold bub

Again the types tell us that

> bub :: ListF K (ListF K (List K)) -> ListF K (List K)

As before, a case analysis on the two levels of ListF in the input
suggests following program:

> bub NilF = NilF
> bub (ConsF a NilF) = ConsF a (In NilF)
> bub (ConsF a (ConsF b x))
>   | a <= b = ConsF a (In (ConsF b x))
>   | a > b  = ConsF b (In (ConsF a x))

I still haven't proved that it sorts, but at least it has a structure
that is motivated by the datatypes involved. Assuming that it works
(which it does), we see that another way to sort is with an unfold,
whose body is a fold.

----------------------------------------------------------------------

In fact, the two inner bodies ins and bub are closely related: the
only differences between them are the occurrences of Out_ in ins and
In in bub, which are inessential (because they are both
isomorphisms). More formally, we can obtain the following program by
simply eliminating the Out_s and Ins (note that in both ins and bub,
the variable x is not further inspected - it is used parametrically -
and so swap has a more general type the either ins or bub):

> swap :: ListF K (ListF K b) -> ListF K (ListF K b)
> swap NilF = NilF
> swap (ConsF a NilF) = ConsF a NilF
> swap (ConsF a (ConsF b x))
>   | a <= b = ConsF a (ConsF b x)
>   | a > b  = ConsF b (ConsF a x)

The original definitions of ins and bub can be reconstructed from swap:

  ins = swap . bimap id out
  bub = bimap id In . swap

and so we could define the two sorting algorithms both in terms of swap:

> isortSwap, bsortSwap :: List K -> Colist K
> isortSwap = fold (unfold (swap . bimap id out))
> bsortSwap = unfold (fold (bimap id In . swap))

This correspondence is explained categorically in the paper "Sorting
with Bialgebras and Distributive Laws" by Ralf Hinze & co in WGP
2012. Alternatively, you might explain the correspondence pictorially,
using sorting networks (as I did in the lecture, but I won't try
drawing the pictures in ASCII).

----------------------------------------------------------------------

We ended up with bubble sort for the sorting algorithm whose outer
structure is an unfold, rather than the selection sort you might
expect. Both have an outer step that involves extracting the minimal
element of the input as the first element of the output. The
difference between them is that bubble sort rearranges the remaining
elements, whereas selection sort leaves them in their original order
(which, of course, has no effect on the correctness of the
algorithm). 

To express selection sort, we need a pattern of computation that makes
use of recursive calls on substructures (as fold does), but also
allows access to the original structures (which fold does not). That
pattern is called a "paramorphism", and it is described in the paper
"Paramorphisms" by Lambert Meertens (Formal Aspects of Computing,
1992). In a nutshell, a paramorphism is like a fold, except that the
body has access to original inputs alongside each result of a
recursive call:

> para :: Bifunctor f => (f a (b, Mu f a) -> b) -> Mu f a -> b

The definition is a generalisation of that of fold:

> para phi (In x) = phi (bimap id (para phi `fork` id) x)

where fork runs two functions in parallel on a common input:

> fork :: (a->b) -> (a->c) -> a -> (b,c)
> (f `fork` g) a = (f a, g a)

Now, selection sort is an unfold-based sort,

> ssort :: List K -> Colist K
> ssort = unfold select

whose body is a paramorphism:

> select :: List K -> ListF K (List K)
> select = para sel

The body of the paramorphism can make use of the original input (y below):

> sel :: ListF K (ListF K (List K), List K) -> ListF K (List K)
> sel NilF = NilF
> sel (ConsF a (NilF, _)) = ConsF a (In NilF)
> sel (ConsF a (ConsF b x, y))
>   | a <= b = ConsF a y
>   | a > b  = ConsF b (In (ConsF a x))

----------------------------------------------------------------------

The insertion sort we defined uses a rather naive insert function: to
insert an element into a sorted list, it traverses the whole list,
even when this is not necessary. For example, to insert the integer 3
into the list [1,2,4,5], it could traverse the [1,2] part, but merely
copy the [4,5] part without traversing it further.

To express the more sophisticated insert function that stops
traversing when the insertion point is located, we need a pattern of
computation that allows step-by-step generation (as unfold does), but
also allows immediate delivery of a complete data structure (which
unfold does not). That pattern is called an "apomorphism", and it is
described in the paper "Functional Programming with Apomorphisms
(Corecursion)" by Varmo Vene and Tarmo Uustalu (Proceedings of the
Estonian Academy of Sciences: Physics, Mathematics, 1998).  In a
nutshell, an apomorphism is like an unfold, except that the body has
the choice for each recursive position to yield a new seed or to
produce an entire recursive subterm:

> apo :: Bifunctor f => (b -> f a (Either b (Nu f a))) -> b -> Nu f a
> apo phi = Out_ . bimap id (apo phi `either` id) . phi

Here, the "Either" datatype in the Haskell libraries encapsulates a
choice: an element of type Either a b is either of the form Left x for
x of type a, or of form Right y for y of type b. And the combinator
"either" combines functions of type a->c and b->c into a function of
type Either a b -> c on the choice.

I encourage you to compare the definitions of para and apo. In fact,
they are precisely dual: each is obtained from the other simply by
reversing appropriate arrows.

Now, smart insertion sort is a fold using smart insertion, which is in
turn defined as an apomorphism:

> smartisort :: List K -> Colist K
> smartisort = fold smartinsert

> smartinsert :: ListF K (Colist K) -> Colist K
> smartinsert = apo smartins

The body smartins of the apomorphism generates a new insertion problem
when a>b, but leaps immediately to the solution when a<=b.

> smartins :: ListF K (Colist K) -> ListF K (Either (ListF K (Colist K)) (Colist K))
> smartins NilF = NilF
> smartins (ConsF a (Out_ NilF)) = ConsF a (Left NilF)
> smartins (ConsF a (Out_ (ConsF b x)))
>   | a <= b = ConsF a (Right (Out_ (ConsF b x)))
>   | a > b  = ConsF b (Left (ConsF a x))

----------------------------------------------------------------------

Datatype genericity comes at a small cost in terms of convenience: we
gain from having a single generic definition of functions such as fold
and para, but pay a small price by having our programs cluttered up with
all the conses, nils, ins and outs. By specialising the definitions to
a particular datatype such as lists, we lose generality but perhaps
gain some clarity. Here are the specialised definitions of the various
sorting algorithms, in case you find them easier to follow.

The datatype of lists in Haskell is written with square brackets, eg
[Int]; this datatype stands in for both List a and Colist a in our
formulation. Here are versions of fold and unfold
for lists:

> foldList :: (Maybe (a,b) -> b) -> [a] -> b
> foldList f [] = f Nothing
> foldList f (a:x) = f (Just (a, foldList f x))

> unfoldList :: (b -> Maybe (a,b)) -> b -> [a]
> unfoldList f b = case f b of 
>   Nothing -> []
>   Just (a,b') -> a : unfoldList f b'

(For the congnoscenti, they are equivalent to Prelude.foldr and
Data.List.unfoldr.)

Here is insertion sort:

> isort' :: [K] -> [K]
> isort' = foldList insert'

> insert' :: Maybe (K, [K]) -> [K]
> insert' = unfoldList ins'

> ins' :: Maybe (K, [K]) -> Maybe (K, Maybe (K, [K]))
> ins' Nothing = Nothing
> ins' (Just (a, [])) = Just (a, Nothing)
> ins' (Just (a, b:x))
>   | a <= b = Just (a, Just (b,x))
>   | a > b  = Just (b, Just (a,x))

You can test it as follows:

*Main> isort' [3,1,4,1,5]
[1,1,3,4,5]

Here is bubblesort:

> bsort' :: [K] -> [K]
> bsort' = unfoldList bubble'

> bubble' :: [K] -> Maybe (K, [K])
> bubble' = foldList bub'

> bub' :: Maybe (K, Maybe (K,[K])) -> Maybe (K, [K])
> bub' Nothing = Nothing
> bub' (Just (a, Nothing)) = Just (a, [])
> bub' (Just (a, Just (b,x)))
>   | a <= b = Just (a, b:x)
>   | a > b  = Just (b, a:x)

Here are paramorphisms for lists, and selection sort:

> paraList :: (Maybe (a, (b, [a])) -> b) -> [a] -> b
> paraList phi [] = phi Nothing
> paraList phi (a:x) = phi (Just (a, (paraList phi x, x)))

> ssort' :: [K] -> [K]
> ssort' = unfoldList select'

> select' :: [K] -> Maybe (K, [K])
> select' = paraList sel'

> sel' :: Maybe (K, (Maybe (K, [K]), [K])) -> Maybe (K, [K])
> sel' Nothing = Nothing
> sel' (Just (a, (Nothing, _))) = Just (a, [])
> sel' (Just (a, (Just (b,x), y)))
>   | a <= b = Just (a, y)
>   | a > b  = Just (b, a:x)

And here are apomorphisms for lists, and smart insertion sort:

> apoList :: (b -> Maybe (a, Either b [a])) -> b -> [a]
> apoList phi b = case phi b of
>   Nothing -> []
>   Just (a, Left b') -> a : apoList phi b'
>   Just (a, Right x) -> a : x

> smartisort' :: [K] -> [K]
> smartisort' = foldList smartinsert'

> smartinsert' :: Maybe (K, [K]) -> [K]
> smartinsert' = apoList smartins'

> smartins' :: Maybe (K, [K]) -> Maybe (K, Either (Maybe (K, [K])) [K])
> smartins' Nothing = Nothing
> smartins' (Just (a, [])) = Just (a, Left Nothing)
> smartins' (Just (a, b:x))
>   | a <= b = Just (a, Right (b:x))
>   | a > b  = Just (b, Left (Just (a,x)))

----------------------------------------------------------------------

