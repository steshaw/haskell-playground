{-# LANGUAGE ExistentialQuantification #-}
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

data Object = forall a. Show a => Object { getObject :: a }

objects = [Object one, Object "hi", Object 'a']

show_ :: [Object] -> String
show_ xs = intercalate ", " $ map (\case (Object o) -> show o) xs

showObj :: [Object] -> String
showObj [] = ""
showObj ((Object x):xs) = show x ++ showObj xs

data Request = forall a. Request (IO a) (MVar a)

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
