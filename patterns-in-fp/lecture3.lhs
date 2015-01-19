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

  ghci lecture3.lhs

at the command line. 

----------------------------------------------------------------------

We have to make use of "generalized algebraic datatypes" in this lecture:

> {-# OPTIONS -XGADTs -XStandaloneDeriving #-}

----------------------------------------------------------------------

Lecture 3: Generic Programming

In contrast to the "parametric datatype-generic programming" we have
seen so far, in which the type or functor parameter is used as a black
box, in this lecture we will cover "ad-hoc datatype-generic
programming", where the parameter is inspected in order to determine
which code to use for the specialization to that parameter. 

There are many approaches to this style of generic programming in
Haskell - as programming techniques, as libraries, as code generators,
and as language extensions. The one we will use is a programming
technique, and is based on the paper "A Lightweight Implementation of
Generics and Dynamics" by James Cheney and Ralf Hinze (Haskell
Workshop, 2002).

We start with a simple collection of polynomial datatype formers:

> data Prod a b   = Pair a b       deriving Show
> data Sum a b    = Inl a | Inr b  deriving Show
> data Unit       = Unit           deriving Show

In the lectures, we also included zero (the empty type) and exponents
(function spaces), because they produce pretty mathematics, but we
omit them here: the zero datatype because it can never be used, and
exponents because they aren't eligible as parameters for the
particular generic functions we will be defining.

However, we will use an embedding/projection technique to let us
incorporate any standard datatype that is representable as a
polynomial:

> data Iso a b = EP {from :: a -> b, to :: b -> a} -- should be inverses
> instance Show (Iso a b) where show (EP _ _) = "<iso>"

For example, Haskell's list datatype [a] is isomorphic to a sum of
products, and that isomorphism can be expressed as a value of a
suitable Iso type:

> isoList :: Iso [a] (Sum Unit (Prod a [a]))
> isoList = EP unpack pack where
>   unpack []               = Inl Unit
>   unpack (x:xs)           = Inr (Pair x xs)
>   pack (Inl Unit)         = []
>   pack (Inr (Pair x xs))  = x:xs

----------------------------------------------------------------------

Now we collect the datatype formers into a "universe" (ie a
type-indexed datatype) of type representations:

> data Rep t where
>    RUnit  ::                      Rep Unit
>    RInt   ::                      Rep Int
>    RChar  ::                      Rep Char
>    RSum   :: Rep a -> Rep b ->    Rep (Sum a b)
>    RProd  :: Rep a -> Rep b ->    Rep (Prod a b)
>    RIso   :: Iso a b -> Rep b ->  Rep a
> deriving instance Show (Rep t)

Each value of some Rep type is a type representation, or "code for a
type"; for example, the value RSum RUnit RInt is a representation of
the type Sum Unit Int. Note that the type of the value RSum RUnit RInt
is not merely Rep, stating that it is a representation of some type,
but Rep (Sum Unit Int), stating explicitly that it is a representation
of the type Sum Unit Int.

Rep is a parametrized type, and a value of type Rep t is a
representation of the type t. However, Rep is a funny parametrized
type: unlike familiar parametrized types such as [t], a value of type
Rep t isn't a container with elements of type t inside it. The type
parameter is really a "type index"; it annotates the type with
additional information, rather than identifying the type of any
contents. The parameter t is called a "phantom type", and Rep t a
"generalized algebraic datatype".

Note that there is no explicit representation of recursive types such
as lists. Instead, we have to infinite codes by indefinite unfolding
of the type isomorphisms. For example, here is a representation of the
type of lists of elements, taking a representation of the element type
as an argument - it is recursive, and if you try to print it out
you'll get lots of output:

> repList :: Rep a -> Rep [a]
> repList ra = RIso isoList (RSum RUnit (RProd ra (repList ra)))

----------------------------------------------------------------------

The point of introducing the universe Rep is that we can define
generic functions by structural recursion over the codes in Rep. For
example, here is a datatype-generic equality function, which takes a
code for a type t, and two arguments of type t, and returns a boolean.

> rEqual :: Rep t -> t -> t -> Bool

We need to give clauses for each case in our universe of codes. For
type constants such as Int and Char we will use built-in definitions,
and for the unit type equality is trivial.

> rEqual RInt           t1  t2  =  t1 == t2
> rEqual RChar          t1  t2  =  t1 == t2
> rEqual RUnit          _   _   =  True    

Equality for sums and for products decomposes the two values being
compared and recurses accordingly. Note that the codes in the universe
are themselves structured, and carefully designed so that they contain
the necessary codes for the recursive calls.

> rEqual (RSum ra rb)   t1  t2  =  case  (t1, t2) of
>                                     (Inl x, Inl y)  -> rEqual ra x y
>                                     (Inr x, Inr y)  -> rEqual rb x y
>                                     _               -> False
> rEqual (RProd ra rb)  t1  t2  =  case  (t1, t2) of
>                                     (Pair x y, Pair x' y') -> rEqual ra x x' && rEqual rb y y'

Finally, for standard types such as lists, we convert them into their
equivalent polynomial form and compare at that type.

> rEqual (RIso ep ra)  t1  t2   =  rEqual ra (from ep t1) (from ep t2)

For example, you can compare two lists of characters with 
rEqual (repList RChar):

  *Main> rEqual (repList RChar) "abc" "def"
  False

----------------------------------------------------------------------

This works, but it's a bit painful to have to specify the type
representations every time. Happily, that's not necessary: after all,
Haskell can infer the types of the last two arguments, and provided
that there is a unique code for each representable type, then using
type class hackery it can generate the appropriate first argument!

We introduce a type class for types with inferrable type
representations ("Repable" here is short for "Representable"):

> class Repable t where rep :: Rep t

Of course, each of our basic types is representable:

> instance Repable Unit where rep = RUnit
> instance Repable Int where rep = RInt
> instance Repable Char where rep = RChar

and constructed types are representable if the types from which they
are constructed are:

> instance (Repable a, Repable b) => Repable (Sum a b) where rep = RSum rep rep
> instance (Repable a, Repable b) => Repable (Prod a b) where rep = RProd rep rep
> instance Repable a => Repable [a] where rep = RIso isoList rep

(Don't worry if you don't understand this type class hackery. It is
quite sophisticated, and it's not the subject of the lecture series.)

Now a more convenient definition of datatype-generic equality can be
given, with inferred type representation argument:

> equal :: Repable t => t -> t -> Bool
> equal t1 t2 = rEqual rep t1 t2

so you can write

  *Main> equal "abc" "def"
  False

  *Main> equal [1,2,3] ([1,2,3] :: [Int])
  True

but equal "abc" [1,2,3] is ill-typed. 

(Note the type annotation "::[Int]" above. This is nothing to do with
generic programming; it's to work around a feature in Haskell. Numeric
constants such as 1 and 2 don't have a fixed type like Int; they can
be of other types too, such as Float. In fact, they are of type Num a
=> a, ie of any numeric type. This means that the type-based
generation of representations doesn't work with an unadorned numeric
constant: we don't have a representation of Float, and if we added
one, then we would have ambiguous representations for the type of
constants such as 1. The type annotation in the example above
restricts the list specifically to be a list of Ints, for which
suitable representations are inferrable.)

----------------------------------------------------------------------

I'll leave you with an exercise: to define datatype-generic encoding
and decoding

  encode :: Repable t => t -> [Bool]
  decode :: Repable t => [Bool] -> t

such that

  *Main> encode "abc"
[True,True,False,False,False,False,True,True,False,True,False,True,False,False,False,True,True,False,True,True,True,False,False,False,True,True,False,False]

  *Main> decode (encode "abc") :: String
  "abc"

Hint: write versions with explicit type representatio arguments first.

  rEncode :: Rep t -> t -> [Bool]

is straightforward, but I recommend that you define

  rDecode :: Rep t -> [Bool] -> Maybe (t, [Bool])

rather than

  rDecode :: Rep t -> [Bool] -> t

because decoding (a) my fail, and (b) will need to return the leftover
input so that subsequent decoders (eg for the second half of a pair)
will have input on which to run.

----------------------------------------------------------------------
