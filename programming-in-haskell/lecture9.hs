import Prelude hiding (getLine, putStr, putStrLn)
import Data.List (unfoldr)

getLine :: IO String
getLine = do
  c <- getChar
  if c == '\n' then
    return []
  else do
    cs <- getLine
    return (c:cs)

type Predicate a = a -> Bool

getUntil :: Predicate Char -> IO String
getUntil p = do
  c <- getChar
  if (p c) then
    return []
  else do
    cs <- getUntil p
    return (c:cs)

getLine' :: IO String
getLine' = getUntil (== '\n')

atob :: Int -> Int -> [Int]
atob a b = [a..b]

atob' :: Int -> Int -> [Int]
atob' a b =
  if (a > b) then []
  else a:(atob' (a+1) b)

gen :: Predicate a -> a -> [a] -> (a -> a) -> [a]
gen p a baseCase step = if (p a) then baseCase else a:(gen p (step a) baseCase step)

atob'' :: Int -> Int -> [Int]
atob'' a b = gen (>b) a [] (+1)

gen' :: Predicate a -> a -> b -> (a -> a) -> (a -> b -> b) -> b
gen' p a baseCase step fred = if (p a) then baseCase else fred a (gen' p (step a) baseCase step fred)

atob''' :: Int -> Int -> [Int]
atob''' a b = gen' (>b) a [] (+1) (:)

{-
getLine'' :: IO String
getLine'' = gen' (== '\n') getChar [] id (:)
-}

type Unfoldr a b = (b -> Maybe (a, b)) -> b -> [a]

atob4 :: Int -> Int -> [Int]
atob4 a b = unfoldr f a
  where
    f :: Int -> Maybe (Int, Int)
    f n = if (n > b) then Nothing else Just (n, n+1)

putStr :: String -> IO ()
putStr []     = return ()
putStr (x:xs) = do putChar x
                   putStr xs

putStr' :: String -> IO ()
putStr' = foldr f (return ())
  where
    f :: Char -> IO () -> IO ()
    f char rest = putChar char >> rest

putStrLn :: String -> IO ()
putStrLn xs = do putStr xs
                 putChar '\n'
