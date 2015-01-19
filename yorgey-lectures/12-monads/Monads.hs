--import Control.Monad
import Control.Applicative
import Prelude hiding (sequence)
import Control.Arrow (first)
import Data.Char (isDigit, isSpace)

check :: Int -> Maybe Int
check n | n < 10    = Just n
        | otherwise = Nothing

halve :: Int -> Maybe Int
halve n | even n    = Just $ n `div` 2
        | otherwise = Nothing

ex01, ex02, ex03 :: Maybe Int
ex01 = pure 7  >>= check >>= halve
ex02 = pure 12 >>= check >>= halve
ex03 = pure 12 >>= halve >>= check

addOneOrTwo :: Int -> [Int]
addOneOrTwo x = [x + 1, x + 2]

ex04 :: [Int]
ex04 = [10, 20, 30] >>= addOneOrTwo

sequence :: Monad m => [m a] -> m [a]
sequence [] = return []
sequence (ma:mas) =
  ma >>= \a ->
  sequence mas >>= \as ->
  return (a:as)

replicateM :: Monad m => Int -> m a -> m [a]
replicateM n ma = sequence (replicate n ma)

--
-- Parsing bits
--

newtype Parser a = Parser { getParser :: String -> Maybe (a, String) }

instance Functor Parser where
  fmap f (Parser rp) = Parser $ \s -> fmap (first f) $ rp s

instance Monad Parser where
  return a = Parser (\s -> Just (a, s))
  Parser rp1 >>= fmb = Parser $ \s ->
    case rp1 s of
      Nothing -> Nothing
      Just (a, s') -> getParser (fmb a) s'

instance Applicative Parser where
  pure = return
  Parser rp1 <*> p2 = Parser $ \s ->
    case rp1 s of
      Nothing -> Nothing
      Just (f, s') -> getParser (f <$> p2) s'

instance Alternative Parser where
  empty = Parser (\_ -> Nothing)
  Parser rp1 <|> Parser rp2 = Parser $ \s -> case rp1 s of
                                                 Nothing -> rp2 s
                                                 r -> r

{-
instance Alternative Parser where
  empty = Parser (const Nothing)
  Parser p1 <|> Parser p2 = Parser $ liftA2 (<|>) p1 p2
-}

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser f
  where
    f [] = Nothing
    f (x:xs)
        | p x       = Just (x, xs)
        | otherwise = Nothing

many1 :: Parser a -> Parser [a]
many1 p = liftA (:) p <*> many p

spaces :: Parser String
spaces = many (satisfy isSpace)

withSpaces :: Parser a -> Parser a
withSpaces p = spaces *> p <* spaces

parseInt :: Parser Int
parseInt = withSpaces $ fmap read $ many1 (satisfy isDigit)

parseLine :: Parser [Int]
parseLine = parseInt >>= \i -> replicateM i parseInt

parseFile :: Parser [[Int]]
parseFile = many parseLine

input :: String
input = "4 78 19 3 44 3 1 7 5 2 3 2"

pl :: Maybe ([[Int]], String)
pl = getParser parseFile input

ns :: Maybe ([Int], String)
ns = getParser (many parseInt) input
