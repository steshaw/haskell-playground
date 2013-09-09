import Control.Monad (foldM, forM_)
import Data.Char (isDigit, ord)

atoi0 :: String -> Int
atoi0 = foldl (\acc n -> fromEnum n - fromEnum '0' + acc * 10) 0

examples = ["", "1", "12", "123", "1234", "Hi", "asdf"]

r1 = map atoi0 examples
p1 = r1 `forM_` (putStrLn.show)

type Digit = Char

digitToInt0 :: Digit -> Maybe Int
digitToInt0 digit = case digit of
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

digitToInt :: Digit -> Maybe Int
digitToInt digit = if isDigit digit then Just (ord digit - ord '0') else Nothing

atoi1 :: String -> Maybe Int
atoi1 "" = Nothing
atoi1 s = foldM f 0 s
  where
    f :: Int -> Digit -> Maybe Int
    f acc digit = do
      n <- digitToInt digit
      return $ n + acc * 10

r2 = map atoi1 examples
p2 = r2 `forM_` (putStrLn.show)

fromBool b a = if b then Just a else Nothing

--
-- Eking out a short version...
--
atoi "" = Nothing
atoi s = foldM f 0 s
  where
    f acc digit = do
      n <- digitToInt digit
      return $ n + acc * 10
    digitToInt digit = fromBool (isDigit digit) (ord digit - ord '0')

r3 = map atoi examples
p3 = r3 `forM_` (putStrLn.show)
