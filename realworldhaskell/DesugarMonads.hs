{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Steshaw
import Prelude hiding (catch)
import Control.Exception

go_1 xs =
  return xs >>= \ (x:xs) -> return x

go_2 xs = do
  (x:xs) <- return xs
  return x

main = do
  a <- go_1 [1..3]
  putStrLn $ "go_1 [1..3] => " ++ show a
  a <- go_2 [1..3]
  putStrLn $ "go_2 [1..3] => " ++ show a

  catch (do
    a <- go_1 ([] :: [Integer])
    putStrLn $ "go_1 [] => " ++ show a)
    (\ (err :: PatternMatchFail) -> do putStrLn $ "Caught error: " ++ show err)
  catch (do
    a <- go_2 ([] :: [Integer])
    putStrLn $ "go_2 [] => " ++ show a)
    (\ (err :: SomeException) -> do putStrLn $ "Caught error: " ++ show err)

    -- FIXME: The intent here is to show the difference between go_1 and go_2.
    -- FIXME: i.e. in the Maybe monad, because fail is called when a pattern match fails,
    -- FIXME: and fail for Maybe monad returns Nothing - this will be *different* from
    -- FIXME: the go_1 case where the failed pattern match always raises an exception.
    -- FIXME: Note that this is easy to test in ghci...
{-
  a <- ((go_1 [1..3]) :: Maybe Integer)
  putStrLn $ "go_1 [1..3] => " ++ show a
  a <- ((go_2 [1..3]) :: Maybe Integer)
  putStrLn $ "go_2 [1..3] => " ++ show a
-}

{-
  catch (do
    (a::Maybe Integer) <- ((go_1 ([] :: [Integer])) :: Maybe Integer)
    --putStrLn $ "go_1 [] => " ++ show a
    putStrLn "ok"
    )
    (\ (err :: PatternMatchFail) -> do putStrLn $ "Caught error: " ++ show err)
  catch (do
    a <- ((go_2 ([] :: [Integer])) :: Maybe Integer)
    --putStrLn $ "go_2 [] => " ++ show a
    putStrLn "ok"
    )
    (\ (err :: SomeException) -> do putStrLn $ "Caught error: " ++ show err)
-}
