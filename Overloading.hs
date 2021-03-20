-- https://stackoverflow.com/a/51056617/482382
-- https://blog.sumtypeofway.com/posts/existential-haskell.html

{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

import Control.Concurrent
import Data.List

class Foo a where
  foo :: a -> IO ()
  foo _ = putStrLn "foo-default"

instance Foo Int

instance Foo Char where
  foo _ = putStrLn "foo-char"

class Foo a => Bar a where
  bar :: a -> IO ()
  bar a = do
    putStrLn "bar-default"
    foo a

instance Bar Int
instance Bar Char where
  -- not possible
  -- foo _ = putStrLn "foo-char"

one = 1 :: Int

hello :: Bar a => a -> IO ()
hello x = do
  putStrLn "foo x"
  foo x
  putStrLn "bar x"
  bar x

------------------------------------------------------------------------

data Request = forall a. Request (IO a) (MVar a)

data Object = forall a. Show a => Object { getObject :: a }

objects = [Object one, Object "hi", Object 'a']

show_ :: [Object] -> String
--                                Cannot replace with `(show . getObject)`.
show_ xs = intercalate ", " $ map (\case (Object o) -> show o) xs

showObj :: [Object] -> String
showObj [] = ""
showObj ((Object x):xs) = show x ++ showObj xs

------------------------------------------------------------------------

data Object2 where
  Object2 :: Show a => a -> Object2

objects2 = [Object2 1, Object2 'a', Object2 "hi"]

show2 :: [Object2] -> String
show2 xs = intercalate ", " $ map (\case (Object2 o) -> show o) xs

------------------------------------------------------------------------

main = do
  putStrLn "foo a"
  foo 'a'
  putStrLn "foo one"
  foo one
  putStrLn "bar a"
  bar 'a'
  putStrLn "bar one"
  bar one
  putStrLn "hello a"
  hello 'a'
  putStrLn "hello one"
  hello one
