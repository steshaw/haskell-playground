import Control.Monad (foldM)

atoi :: String -> Int
atoi = foldl (\acc n -> fromEnum n - fromEnum '0' + acc * 10) 0

examples = ["", "1", "12", "123", "a", "asdf"]

r1 = map atoi examples

digitToInt digit = case digit of
  '0' -> Just 0
  '1' -> Just 1
  '2' -> Just 2
  '3' -> Just 3
  '4' -> Just 4
  '5' -> Just 5
  '6' -> Just 6
  '7' -> Just 7
  '8' -> Just 8
  '9' -> Just 9
  _   -> Nothing

type Digit = Char

atoi' :: String -> Maybe Int
atoi' "" = Nothing
atoi' s = foldM f 0 s
  where
    f :: Int -> Digit -> Maybe Int
    f acc digit = do
      n <- digitToInt digit
      return $ n + acc * 10

r2 = map atoi' examples
