--
-- See http://john-millikin.com/articles/monad-is-not-difficult/
--
module Filter where

import Prelude hiding (map, concat, concatMap, filter, length, foldl)

import qualified Data.List as DL


data List a = Cons a (List a) | End
  deriving (Show)

instance Monad List where
  return x = Cons x End
  list >>= f = concatMap f list

toList :: [a] -> List a
toList = DL.foldl' (flip Cons) End

append :: List a -> List a -> List a
append End         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

concat :: List (List a) -> List a
concat End = End
concat (Cons x xs) = append x (concat xs)

map :: (a -> b) -> List a -> List b
map _ End         = End
map f (Cons x xs) = Cons (f x) (map f xs)

concatMap :: (a -> List b) -> List a -> List b
concatMap f x = concat $ map f x

filter :: (a -> Bool) -> List a -> List a
filter p list = do
  x <- list
  if p x
    then return x
    else End

foldl :: (a -> b -> a) -> a -> List b -> a
foldl _ initial End         = initial
foldl f initial (Cons x xs) = foldl f (f initial x) xs

foldl' :: (a -> b -> a) -> a -> List b -> a
foldl' _ initial End         = initial
foldl' f initial (Cons x xs) =
  let fx = f initial x
  in seq fx $ foldl f fx xs

length :: List a -> Integer
length = foldl (\a _ -> a + 1) 0

length' :: List a -> Integer
length' = foldl' (\a _ -> a + 1) 0

veryBigList :: List Integer
veryBigList = let xs = toList [1..1000000] in seq xs xs
