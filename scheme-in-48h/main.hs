module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Applicative ((<**>))
import Numeric
import Data.Char

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | Char Char
  | String String
  | Bool Bool
  deriving (Show)

dq = '"'

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> show s

parseChar :: Parser LispVal
parseChar = try $ do
  char '#'; char '\\'
  named "space" ' '
    <|> named "newline" '\n'
    <|> otherChar
  where
    named :: String -> Char -> Parser LispVal
    named s c = caseInsensitiveString s >> (return $ Char c)
    otherChar :: Parser LispVal
    otherChar = anyChar >>= \c -> (return $ Char c)

parseString :: Parser LispVal
parseString = do
  char dq
  x <- many (noneOf [dq] <|> (char '\\' >> oneOf [dq, 'n', 'r', 't', '\\']))
  char dq
  return $ String x

parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _    -> Atom atom

radix :: Char -> String -> (String -> Integer) -> Parser Integer
radix c validDigits convert = do
  try $ do
    char '#'
    char c
    ds <- many1 (oneOf validDigits)
    return $ convert ds

number :: Parser Integer
number = do
  ds <- many1 digit
  return $ read ds

float :: Parser Float
float = try $ do
  as <- many1 digit
  char '.'
  bs <- many1 digit
  return $ read (as ++ "." ++ bs)

extractNum [(num, "")] = num

(===>) = flip fmap

parseNumber :: Parser LispVal
parseNumber = (
      radix 'b' ['0'..'1'] error -- FIX
  <|> radix 'o' ['0'..'7'] (extractNum . readOct)
  <|> radix 'd' ['0'..'9'] (extractNum . readDec)
  <|> radix 'x' (['0'..'9'] ++ ['A'..'F']) (extractNum . readHex)
  <|> number
  ) ===> Number

parseExpr :: Parser LispVal
parseExpr =
      (fmap Float float)
  <|> parseNumber
  <|> parseChar
  <|> parseString
  <|> parseAtom

symbol :: Parser Char
symbol = oneOf "!#$%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> "No match: " ++ show err
  Right val -> "Ok: " ++ show val

main :: IO ()
main = do
  args <- getArgs
  case args of
    [expr] -> putStrLn $ readExpr expr
