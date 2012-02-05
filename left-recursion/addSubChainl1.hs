--
-- Adapted from http://stuckinaninfiniteloop.blogspot.com.au/2011/10/left-recursion-in-parsec.html
--

import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), (<*))

data Exp 
  = Const Int
  | Add Exp Exp
  | Sub Exp Exp

instance Show (Exp) where
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

top :: Parser Exp
top = spaces *> term <* eof

term :: Parser Exp
term = chainl1 parseConstant parseOperation

parseOperation :: Parser (Exp -> Exp -> Exp)
parseOperation = do 
  symbol <- char '+' <|> char '-'
  spaces
  case symbol of
    '+' -> return Add
    '-' -> return Sub
    _   -> error "Unexpected. Should always be a '+' or '-' here"

parseConstant :: Parser Exp
parseConstant = do 
  xs <- many1 digit
  spaces
  return $ Const (read xs :: Int)
