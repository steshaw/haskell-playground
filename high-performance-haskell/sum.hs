import Prelude hiding (sum, product, foldl)

-- not stack friendly
noTailSum :: [Int] -> Int
noTailSum [] = 0
noTailSum (x:xs) = x + noTailSum xs

noTailProduct :: [Int] -> Int
noTailProduct [] = 1
noTailProduct (x:xs) = x * noTailSum xs

tailSum :: [Int] -> Int
tailSum xs = sum' 0 xs
  where
    sum' acc [] = acc
    sum' acc (x : xs) = sum' (acc + x) xs

tailProduct :: [Int] -> Int
tailProduct xs = product' 1 xs
  where
    product' acc []     = acc
    product' acc (x:xs) = product' (acc * x) xs

foldl :: (a -> b -> a) -> a -> [b] -> a
foldl f init xs = aux init xs
  where
    aux acc []     = acc
    aux acc (x:xs) = aux (acc `f` x) xs

fSum = foldl (+) 0
fProduct = foldl (*) 1

main :: IO ()
main = do
  print $ tailSum [1..10000000]
  print $ tailProduct [1..1000]
  print $ fSum [1..10000000]
  print $ fProduct [1.1000]
