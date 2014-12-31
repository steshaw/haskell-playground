module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Char (toUpper, toLower)
import Control.Monad (forever)
import System.IO (isEOF, hFlush, stdout)
import Control.Exception (
  SomeException(..),
  AsyncException(..),
  catch, throw
  )
import Control.Monad.Error
import Data.List (foldl1')

data Val
  = Symbol String
  | List [Val]
  | DottedList [Val] Val
  | Number Integer
  | Float Float
  | Char Char
  | String String
  | Boolean Bool

instance Show Val where show = showVal

data Err
  = NumArgs Integer [Val]
  | TypeMismatch String Val
  | Parser ParseError
  | BadSpecialForm String Val
  | NotFunction String String
  | UnboundVar String String
  | Default String

showErr :: Err -> String
showErr (UnboundVar msg var) = msg ++ ": " ++ var
showErr (BadSpecialForm msg form) = msg ++ ": " ++ show form
showErr (NotFunction msg proc) = msg ++ ": " ++ show proc
showErr (NumArgs expected found) = 
  "Expected " ++ show expected ++ 
  " args; found values " ++ unwordsList found
showErr (TypeMismatch expected found) =
  "Invalid type: expected " ++ expected ++ 
  ", found " ++ show found
showErr (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show Err where show = showErr

instance Error Err where
  noMsg = Default "An error has occurred"
  strMsg = Default

type ThrowsErr = Either Err

trapError action = catchError action (return . show)

extractValue :: ThrowsErr a -> a
extractValue (Right v) = v

dq :: Char
dq = '"'

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> show s

parseChar :: Parser Val
parseChar = try $ do
  char '#'; char '\\'
  named "space" ' '
    <|> named "newline" '\n'
    <|> otherChar
  where
    named :: String -> Char -> Parser Val
    named s c = caseInsensitiveString s >> (return $ Char c)
    otherChar :: Parser Val
    otherChar = anyChar >>= \c -> (return $ Char c)

parseString :: Parser Val
parseString = do
  char dq
  x <- many (noneOf [dq] <|> (char '\\' >> oneOf [dq, 'n', 'r', 't', '\\']))
  char dq
  return $ String x

parseSymbol :: Parser Val
parseSymbol = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let s = first:rest
  return $ case s of
    "#t" -> Boolean True
    "#f" -> Boolean False
    _    -> Symbol s

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

parseNumber :: Parser Val
parseNumber = (
      radix 'b' ['0'..'1'] error -- FIX
  <|> radix 'o' ['0'..'7'] (extractNum . readOct)
  <|> radix 'd' ['0'..'9'] (extractNum . readDec)
  <|> radix 'x' (['0'..'9'] ++ ['A'..'F']) (extractNum . readHex)
  <|> number
  ) ===> Number

parseList :: Parser Val
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser Val
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser Val
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Symbol "quote", x]

parseExpr :: Parser Val
parseExpr =
      (fmap Float float)
  <|> parseNumber
  <|> parseChar
  <|> parseString
  <|> parseSymbol
  <|> parseQuoted
  <|> do char '('
         l <- try parseList <|> parseDottedList
         char ')'
         return l

symbol :: Parser Char
symbol = oneOf "!#$%|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

showVal :: Val -> String
showVal (String s) = show s --"\"" ++ contents ++ "\""
showVal (Symbol name) = name
showVal (Number n) = show n
showVal (Float f) = show f
showVal (Char ' ') = "#\\space"
showVal (Char '\n') = "#\\newline"
showVal (Char c) = "#" ++ [c]
showVal (Boolean True) = "#t"
showVal (Boolean False) = "#f"
showVal (List cs) = "(" ++ unwordsList cs ++ ")"
showVal (DottedList hd tl) = "(" ++ unwordsList hd ++ "." ++ showVal tl ++ ")"

eval :: Val -> Val
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Float _) = val
eval val@(Char _) = val
eval val@(Boolean _) = val
eval val@(Symbol _) = val
eval (List [Symbol "quote", val]) = val
eval (List (Symbol f : args)) = apply f $ map eval args

apply :: String -> [Val] -> Val
apply f args = 
  maybe (Boolean False) ($ args) $ lookup f primitives -- TODO: error-handling

primitives :: [(String, [Val] -> Val)]
primitives =
  [("+", numericBinop (+))
  ,("-", numericBinop (-))
  ,("*", numericBinop (*))
  ,("/", numericBinop (div))
  ,("mod", numericBinop mod)
  ,("quotient", numericBinop quot)
  ,("remainder", numericBinop rem)
  ,("symbol?", predicate isSymbol)
  ,("string?", predicate isString)
  ,("list?", predicate isList)
  ,("boolean?", predicate isBoolean)
  ,("number?", predicate isNumber)
  ,("real?", predicate isReal)
  ,("char?", predicate isChar)
  ,("null?", predicate isNull)
  ,("symbol->string", f1 symbolToString)
  ,("string->symbol", f1 stringToSymbol)
  ]

type PrimF = [Val] -> Val

type Predicate = Val -> Bool

f1 :: (Val -> Val) -> PrimF
f1 f [obj] = f obj

predicate :: Predicate -> PrimF
predicate p [obj] = Boolean $ p obj

isSymbol :: Predicate
isSymbol (Symbol a) = True
isSymbol _        = False

isString :: Predicate
isString (String a) = True
isString _          = False

-- FIX: Must inspect structure of list, ending with '().
isList :: Predicate
isList (List _)         = True
--isList (DottedList _ _) = True
isList _                = False

isBoolean :: Predicate
isBoolean (Boolean _) = True
isBoolean _           = False

isNumber :: Predicate
isNumber (Number _)  = True

isReal :: Predicate
isReal (Float _)   = True
isReal _           = False

isChar :: Predicate
isChar (Char _)   = True
isChar _          = False

isNull :: Predicate
isNull (List []) = True
isNull _         = False

numericBinop :: (Integer -> Integer -> Integer) -> PrimF
numericBinop op params = Number $ foldl1' op $ map unpackNum params

unpackNum :: Val -> Integer
unpackNum (Number n) = n
unpackNum _          = 0 -- FIX

symbolToString :: Val -> Val
symbolToString (Symbol a) = String a
symbolToString _        = Boolean False -- FIX: raise exception

stringToSymbol :: Val -> Val
stringToSymbol (String s) = Symbol s
stringToSymbol _          = Boolean False -- FIX: raise exception

unwordsList :: [Val] -> String
unwordsList = unwords . map showVal

-- read+eval
re :: String -> ThrowsErr Val
re input = case parse parseExpr "schemey" input of
  Left err -> throwError $ Parser err
  Right val -> return $ eval val

p (Left err) = putStrLn $ show err
p (Right v)  = putStrLn $ show v

-- read+eval+print
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
          else p (re expr) >> return False

-- read+eval+print+loop
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
    [expr] -> p $ re expr
    _      -> usage
