--
-- Enough of a monadic parser to try out Monad 'fail'.
--

data Parser a = Parser (String -> [(a, String)])

runParser (Parser f) = f

identity :: a -> Parser a
identity a = Parser (\s -> [(a, s)])

bind :: Parser a -> (a -> Parser b) -> Parser b
first `bind` second = Parser chain
  where
    chain i = case runParser first i of
      [] -> []
      [(a, s)] -> runParser (second a) s

instance Monad Parser where
  return = identity
  (>>=) = bind

item :: Parser Char
item = Parser (\(x:xs) -> [(x, xs)])

notSpace p = do
  a <- p
  if (a == ' ')
    then fail "cannot have space"
    else return a

asdf xs = do
  x <- xs
  if (x == 0)
    then fail "cannot have zero"
    else return x
