--
-- Compares OCaml list definitions in Haskell
--

{-
type 'a list =
  | Nil
  | Cons of 'a * 'a list;;
-}

data List a =
    Nil
  | Cons a (List a)

data List' a =
    Nil'
  | a `Cons'` List' a
