--
-- See http://stuckinaninfiniteloop.blogspot.com.au/2011/10/left-recursion-in-parsec.html
--

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Expr
import Control.Applicative ((<*), (*>))

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

--
-- Adapted from buildExpressionParser example at
--
--   http://research.microsoft.com/en-us/um/people/daan/download/parsec/parsec.html
--
term :: Parser Exp
term = buildExpressionParser table parseConstant
  where
    table        = [[op "+" Add AssocLeft, op "-" Sub AssocLeft]]
    op s f assoc = Infix (do { _ <- string s <* spaces; return f }) assoc

parseConstant :: Parser Exp
parseConstant = do
  xs <- many1 digit <* spaces
  return $ Const (read xs :: Int)
