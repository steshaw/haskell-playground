
-- Exercise 1: Wholemeal programming

fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1_ :: [Integer] -> Integer
fun1_ = product . map ((-) 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n    = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

fun2_ :: Integer -> Integer
fun2_ o
  | o <= 0    = sum $ iterate f o -- loops indefinitely (to match fun2)
  | otherwise = go o
  where
    go = sum . takeWhile (>0) . filter even . iterate f
    f 1 = 0
    f n
      | even n    = n `div` 2
      | otherwise = 3 * n + 1

