import Prelude hiding (getLine, putStr, putStrLn)
import Data.List (unfoldr)
import GHC.IO.Handle (hSetEcho)
import IO (stdin)

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

getCh :: IO Char
getCh = do
  hSetEcho stdin False
  c <- getChar
  hSetEcho stdin True
  return c

sgetLine :: IO String
sgetLine = do
  x <- getCh
  if x == '\n' then do
    putChar x
    return []
  else do
    putChar '-'
    xs <- sgetLine
    return (x:xs)

hangman :: IO ()
hangman = do
  putStr "Think of a word: "
  word <- sgetLine
  putStrLn "Try to guess it: "
  guess word

guess :: String -> IO ()
guess word = do
  putStr "> "
  xs <- getLine
  if xs == word then do
    putStrLn "You got it!"
  else do
    putStr (diff' word xs)
    guess word

-- What I thought 'diff' would be.
-- put a '*' where the person typed an incorrect character
diff :: String -> String -> String
diff _ [] = []
diff [] try = (const 'x') `map` try
diff (c:correct) (t:try) = (if c == t then c else 'x') : (diff correct try)

-- diff as per the lecture
diff' :: String -> String -> String
diff' xs ys =
  [if elem x ys then x else '-' | x <- xs]
