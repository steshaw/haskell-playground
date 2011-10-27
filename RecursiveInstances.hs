--
-- From http://stackoverflow.com/questions/7828072/how-does-haskell-printf-work/7828634#7828634
--

{-# LANGUAGE FlexibleInstances #-}

module Main where

class FooType a where
  bar :: IO () -> a

instance FooType (IO ()) where
  bar = id

instance (Show x, FooType r) => FooType (x -> r) where
  bar s x = bar (s >> print x)

foo :: FooType a => a
foo = bar (return ())

eg1 :: IO ()
eg1 = foo 3

eg2 :: IO ()
eg2 = foo 3 "hello"

eg3 :: IO ()
eg3 = foo 3 "hello" True

eg4 = foo 3 :: IO () -- type annotation required here...

main = do
  eg1
  eg2
  eg3
  eg4
