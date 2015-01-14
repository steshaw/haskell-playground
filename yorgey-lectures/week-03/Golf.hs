module Golf where

--
-- Exercise 1 Hopscotch
--

skips :: [a] -> [[a]]
skips xs = [everyNth n xs | n <- [1 .. length xs]]
  where
    everyNth :: Int -> [a] -> [a]
    everyNth n as
      | n <= length as = (last $ take n as) : everyNth n (drop n as)
      | otherwise      = []

-- tests
skips1 :: Bool
skips1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
skips2 :: Bool
skips2 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
skips3 :: Bool
skips3 = skips [1 :: Int] == [[1]]
skips4 :: Bool
skips4 = skips [True, False] == [[True, False], [False]]
skips5 :: Bool
skips5 = skips ([] :: [Int]) == []
skipsTests :: [Bool]
skipsTests = [skips1, skips2, skips3, skips4, skips5]
skipsAll :: Bool
skipsAll = all (==True) skipsTests

--
-- Exercise 2 Local maxima
--

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : ns)
  | a < b && b > c = b : localMaxima (b:c:ns)
  | otherwise      = localMaxima (b:c:ns)
localMaxima _ = []

localMaxima1 :: Bool
localMaxima1 = localMaxima [2, 9, 5, 6, 1] == [9, 6]
localMaxima2 :: Bool
localMaxima2 = localMaxima [2, 3, 4, 1, 5] == [4]
localMaxima3 :: Bool
localMaxima3 = localMaxima [1,2,3,4,5] == []
localMaximaTests :: [Bool]
localMaximaTests = [localMaxima1, localMaxima2, localMaxima3]
localMaximaAll :: Bool
localMaximaAll = all (==True) localMaximaTests
