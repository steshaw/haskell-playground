{- Adapted from http://mth.io/talks/adt.pdf -}

{-# LANGUAGE RankNTypes #-}

data Boolean = B {
  fold :: forall a. a -> a -> a
}

true :: Boolean
true  = B (\t _ -> t)

false :: Boolean
false = B (\_ f -> f)

and :: Boolean -> Boolean -> Boolean
and m n = fold m n m
or :: Boolean -> Boolean -> Boolean
or m n  = fold m m n
not :: Boolean -> Boolean
not m   = fold m true false
xor :: Boolean -> Boolean -> Boolean
xor m n = fold m (fold n false true) (fold n false true)
