{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wcompat #-}
{-# OPTIONS_GHC -Widentities #-}
{-# OPTIONS_GHC -Wincomplete-uni-patterns #-}
{-# OPTIONS_GHC -Wincomplete-record-updates #-}
{-# OPTIONS_GHC -Wredundant-constraints #-}
{-# OPTIONS_GHC -Wmissing-export-lists #-}
{-# OPTIONS_GHC -Wpartial-fields #-}
{-# OPTIONS_GHC -Wmissing-deriving-strategies #-}

{-# LANGUAGE LambdaCase #-}

module Runs (main) where

import Prelude hiding (head)

import Control.Arrow ((&&&))
import Control.Monad (forM_, when)
import Data.Semigroup (stimes)
import GHC.Stack
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL
import qualified Prelude as P

runs0 :: Eq x => [x] -> [(x, Integer)]
runs0 xs = L.reverse $ L.foldl' f [] xs
  where
    f [] a = [(a, 1)]
    f ((x,count) : ys) a =
      if a == x then
        (x, count + 1) : ys
      else
        (a, 1) : (x, count) : ys

runs1 :: Eq x => [x] -> [(x, Integer)]
runs1 [] = []
runs1 (x : xs) =
  let (xs1, remaining) = span (== x) xs
  in (x, L.genericLength xs1 + 1) : runs1 remaining

runs2 :: Eq x => [x] -> [(x, Integer)]
runs2 = map foo . L.group
 where
  foo :: [a] -> (a, Integer)
  foo xs@(x : _) = (x, L.genericLength xs)
  foo [] = undefined -- :-o

unsafeHeadXXX :: [a] -> a
unsafeHeadXXX = P.head

runs2a :: Eq x => [x] -> [(x, Integer)]
runs2a = map f . L.group
  where
    f s = (unsafeHeadXXX s, L.genericLength s)

runs2b :: Eq x => [x] -> [(x, Integer)]
runs2b = map (unsafeHeadXXX &&& L.genericLength) . L.group

runs3 :: Eq x => [x] -> [(x, Integer)]
runs3 = map foo . NEL.group
 where
  foo :: NEL.NonEmpty a -> (a, Integer)
  foo xs = case NEL.uncons xs of
    (x, Nothing) -> (x, 1)
    (x, Just xs') -> (x, 1 + L.genericLength (NEL.toList xs'))

shouldEq :: (HasCallStack, Eq a, Show a) => a -> a -> IO ()
shouldEq a b =
  when (a /= b) $ do
    let msg = "Oh dear, a should equal b:"
    putStrLn msg
    putStrLn $ "Expected: " <> show a
    putStrLn $ "Actual  : " <> show b
    putStrLn (prettyCallStack callStack)

s1 :: String
s1 = "aaaabbbacc"
rle1 :: [(Char, Integer)]
rle1 = [('a', 4), ('b', 3), ('a', 1), ('c', 2)]
s2 :: String
s2 = "aaaabbbcca"
rle2 :: [(Char, Integer)]
rle2 = [('a', 4), ('b', 3), ('c', 2), ('a', 1)]

testEg1 :: HasCallStack => (String -> [(Char, Integer)]) -> IO ()
testEg1 f = f s1 `shouldEq` rle1

testEg2 :: HasCallStack => (String -> [(Char, Integer)]) -> IO ()
testEg2 f = f s2 `shouldEq` rle2

unRLE :: [(x, Integer)] -> [x]
unRLE = concatMap (\(c, n) -> stimes n [c])

main :: IO ()
main = do
  unRLE rle1 `shouldEq` s1
  unRLE rle2 `shouldEq` s2

  let runss = [runs0, runs1, runs2, runs2a, runs2b, runs3]
  let tests = [testEg1, testEg2]

  forM_ tests $ \tf -> do
    forM_ runss $ \runs -> do
      tf runs

  let tfRuns = do
        tf <- tests
        runs <- runss
        pure (tf, runs)
  forM_ tfRuns $ \case (tf, runs) -> tf runs

  sequence_ $ do
    tf <- tests
    runs <- runss
    pure $ tf runs

  sequence_ $ do
    tf <- tests
    tf <$> runss
