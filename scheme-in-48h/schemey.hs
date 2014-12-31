module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Char
import Control.Monad (forever)
import System.IO (isEOF, hFlush, stdout)
import Control.Exception (
  SomeException(..),
  AsyncException(..),
  catch, throw
  )
import Data.List (foldl1')

data LispVal
  = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | Float Float
  | Char Char
  | String String
  | Bool Bool
--  deriving (Show)

dq :: Char
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

parseList :: Parser LispVal
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

parseExpr :: Parser LispVal
parseExpr =
      (fmap Float float)
  <|> parseNumber
  <|> parseChar
  <|> parseString
  <|> parseAtom
  <|> parseQuoted
  <|> do char '('
         l <- try parseList <|> parseDottedList
         char ')'
         return l

symbol :: Parser Char
symbol = oneOf "!#$%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

showVal :: LispVal -> String
showVal (String s) = show s --"\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Char ' ') = "#\\space"
showVal (Char '\n') = "#\\newline"
showVal (Char c) = "#" ++ [c]
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List cs) = "(" ++ unwordsList cs ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ "." ++ showVal tl ++ ")"

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Char _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = 
  maybe (Bool False) ($ args) $ lookup f primitives -- TODO: error-handling

primitives :: [(String, [LispVal] -> LispVal)]
primitives =
  [("+", numericBinop (+))
  ,("-", numericBinop (-))
  ,("*", numericBinop (*))
  ,("/", numericBinop (div))
  ,("mod", numericBinop mod)
  ,("quotient", numericBinop quot)
  ,("remainder", numericBinop rem)
  ]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1' op $ map unpackNum params

unpackNum :: LispVal -> Integer
unpackNum (Number n) = n
unpackNum _          = 0 -- FIX

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

re :: String -> String
re input = case parse parseExpr "schemey" input of
  Left err -> "No match: " ++ show err
  Right val -> "Ok: " ++ ((showVal . eval) val)

rep :: IO Bool
rep = do
  putStr "schemey> " >> hFlush stdout
  eof <- isEOF
  if eof
     then putStrLn "" >> return True
     else do
       expr <- getLine
       if null expr
          then return False
          else putStrLn (re expr) >> return False

repl :: IO ()
repl = do
  quit <- rep `catch` onUserInterrupt
  if quit then return () else repl

onUserInterrupt UserInterrupt = putStrLn "\nquitting..." >> return True
onUserInterrupt e = throw e

usage :: IO ()
usage = putStrLn $ "usage: schemey [scheme-expression]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    [expr] -> putStrLn $ re expr
    _      -> usage
