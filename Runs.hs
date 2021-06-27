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

import Control.Arrow ((&&&))
import Control.Monad (forM_)
import qualified Data.List as L
import qualified Data.List.NonEmpty as NEL

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

runs2a :: Eq x => [x] -> [(x, Integer)]
runs2a = map (head {- :-o -} &&& L.genericLength) . L.group

runs3 :: Eq x => [x] -> [(x, Integer)]
runs3 = map foo . NEL.group
 where
  foo :: NEL.NonEmpty a -> (a, Integer)
  foo xs = case NEL.uncons xs of
    (x, Nothing) -> (x, 1)
    (x, Just xs') -> (x, 1 + L.genericLength (NEL.toList xs'))

testEg1 :: (String -> [(Char, Integer)]) -> Bool
testEg1 f = f "aaaabbbacc" == [('a', 4), ('b', 3), ('a', 1), ('c', 2)]

testEg2 :: (String -> [(Char, Integer)]) -> Bool
testEg2 f = f "aaaabbbcca" == [('a', 4), ('b', 3), ('c', 2), ('a', 1)]

main :: IO ()
main = do
  let runss = [runs0, runs1, runs2, runs2a, runs3]
  let tests = [testEg1, testEg2]

  forM_ tests $ \tf -> do
    forM_ runss $ \runs -> do
      print $ tf runs

  let tfRuns = do
        tf <- tests
        runs <- runss
        pure (tf, runs)
  forM_ tfRuns $ \case (tf, runs) -> print (tf runs)

  sequence_ $ do
    tf <- tests
    runs <- runss
    pure $ print $ tf runs

  sequence_ $ do
    tf <- tests
    print . tf <$> runss
