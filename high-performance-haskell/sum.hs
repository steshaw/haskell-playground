import Prelude hiding (foldl)

-- not stack friendly
noTailSum :: Num a => [a] -> a
noTailSum [] = 0
noTailSum (x:xs) = x + noTailSum xs

noTailProduct :: Num a => [a] -> a
noTailProduct [] = 1
noTailProduct (x:xs) = x * noTailProduct xs

tailSum :: Num a => [a] -> a
tailSum xs = sum' 0 xs
  where
    sum' acc [] = acc
    sum' acc (x : xs) = sum' (acc + x) xs

tailProduct :: Num a => [a] -> a
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

foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f init xs = aux init xs
  where
    aux acc []     = acc
    aux acc (x:xs) = let y = acc `f` x
                     in y `seq` (aux y xs)

sfSum = foldl' (+) 0
sfProduct = foldl' (*) 1

main :: IO ()
main = do
  let xs = [1 .. 10000000]
  let ys = [1 .. 1000]
  print "tail"
  print $ tailSum xs
  print $ tailProduct ys
  print "left fold"
  print $ fSum xs
  print $ fProduct ys
  print "strict left fold"
  print $ sfSum xs
  print $ sfProduct xs
