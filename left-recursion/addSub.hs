--
-- addSub Parsec parser with left-recursion
--
-- The parser is (of course) broken. Just here for comparison with the solutions.
--

import Text.ParserCombinators.Parsec
import Control.Applicative ((*>), (<*))

data Exp
  = Const Int
  | Add Exp Exp
  | Sub Exp Exp
  deriving (Show)

{-
instance Show (Exp) where
  show (Const n) = show n
  show (Add l r) = "(" ++ show l ++ "+" ++ show r ++ ")"
  show (Sub l r) = "(" ++ show l ++ "-" ++ show r ++ ")"
-}

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
term = int <|> add <|> sub

add :: Parser Exp
add = do
  e1 <- term
  _ <- char '+'
  spaces
  e2 <- term
  return $ Add e1 e2

sub :: Parser Exp
sub = do
  e1 <- term
  _ <- char '+'
  spaces
  e2 <- term
  return $ Sub e1 e2

int :: Parser Exp
int = do
  digits <- many1 digit
  spaces
  return $ Const $ ((read digits) :: Int)
