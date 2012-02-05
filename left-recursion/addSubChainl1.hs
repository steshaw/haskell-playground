--
-- See http://stuckinaninfiniteloop.blogspot.com.au/2011/10/left-recursion-in-parsec.html
--

import Text.ParserCombinators.Parsec

data IntExp = Const Int
            | Add IntExp IntExp
            | Sub IntExp IntExp

instance Show (IntExp) where
  show (Const n) = show n
  show (Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
  show (Sub l r) = "(" ++ show l ++ "-" ++ show r ++ ")"

run :: Show a => Parser a -> String -> IO ()
run p input =
  case parse p "" input of
    Left err ->
      do putStr "parse error at "
         print err
    Right x -> print x

parseIntExp :: Parser IntExp
parseIntExp =
  chainl1 parseConstant parseOperation

parseOperation :: Parser (IntExp -> IntExp -> IntExp)
parseOperation =
  do spaces
     symbol <- char '+' <|> char '-'
     spaces
     case symbol of
       '+' -> return Add
       '-' -> return Sub

parseConstant :: Parser IntExp
parseConstant =
  do xs <- many1 digit
     return $ Const (read xs :: Int)
