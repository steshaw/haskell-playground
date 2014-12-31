module Main where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric
import Data.Char (toUpper, toLower)
import System.IO (isEOF, hFlush, stdout)
import Control.Exception (
  AsyncException(..),
  catch, throw
  )
import Control.Monad.Except
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
showErr (Default s) = "Error: " ++ s

instance Show Err where show = showErr

-- instance Except Err where
--  noMsg = Default "An error has occurred"
--  strMsg = Default

type ThrowsErr = Either Err

trapError :: (MonadError e m, Show e) => m String -> m String
trapError action = catchError action (return . show)

extractValue :: ThrowsErr a -> a
extractValue (Right v) = v
extractValue _         = error "should be unreachable"

dq :: Char
dq = '"'

caseInsensitiveChar :: Char -> Parser Char
caseInsensitiveChar c = char (toLower c) <|> char (toUpper c)

caseInsensitiveString :: String -> Parser String
caseInsensitiveString s = try (mapM caseInsensitiveChar s) <?> show s

parseChar :: Parser Val
parseChar = try $ do
  _ <- char '#' >> char '\\'
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
  _ <- char dq
  x <- many (noneOf [dq] <|> (char '\\' >> oneOf [dq, 'n', 'r', 't', '\\']))
  _ <- char dq
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
    _ <- char '#' >> char c
    ds <- many1 (oneOf validDigits)
    return $ convert ds

number :: Parser Integer
number = do
  ds <- many1 digit
  return $ read ds

float :: Parser Float
float = try $ do
  as <- many1 digit
  _ <- char '.'
  bs <- many1 digit
  return $ read (as ++ "." ++ bs)

extractNum :: [(t, [Char])] -> t
extractNum [(num, "")] = num
extractNum _           = error "invalid number"

parseNumber :: Parser Val
parseNumber = (
      radix 'b' ['0'..'1'] error -- FIX
  <|> radix 'o' ['0'..'7'] (extractNum . readOct)
  <|> radix 'd' ['0'..'9'] (extractNum . readDec)
  <|> radix 'x' (['0'..'9'] ++ ['A'..'F']) (extractNum . readHex)
  <|> number
  ) ===> Number
    where
      (===>) = flip fmap

parseList :: Parser Val
parseList = fmap List $ sepBy parseExpr spaces

parseDottedList :: Parser Val
parseDottedList = do
  hd <- endBy parseExpr spaces
  tl <- char '.' >> spaces >> parseExpr
  return $ DottedList hd tl

parseQuoted :: Parser Val
parseQuoted = do
  _ <- char '\''
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
  <|> do _ <- char '('
         l <- try parseList <|> parseDottedList
         _ <- char ')'
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

eval :: Val -> ThrowsErr Val
eval val@(String _)               = return val
eval val@(Number _)               = return val
eval val@(Float _)                = return val
eval val@(Char _)                 = return val
eval val@(Boolean _)              = return val
eval val@(Symbol _)               = return val
eval (List [Symbol "quote", val]) = return val
eval (List (Symbol f : args))     = mapM eval args >>= \as -> apply f as
-- TODO: eval DottedList
--eval (DottedList _ _) = error "Implement eval on DottedList" -- FIX
eval badForm = throwError $ BadSpecialForm "Unrecognised special form" badForm

apply :: String -> [Val] -> ThrowsErr Val
apply f args =
  maybe e ($ args) $ lookup f primitives
  where
    e = throwError $ NotFunction "Unrecognised primitive" f

primitives :: [(String, [Val] -> ThrowsErr Val)]
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
  ,("symbol->string", f1' symbolToString')
  ,("string->symbol", f1' stringToSymbol')
  ]

type PrimF = [Val] -> ThrowsErr Val

type Predicate = Val -> Bool

f1' :: (Val -> ThrowsErr Val) -> PrimF
f1' f [obj] = f obj
f1' _ args  = throwError $ NumArgs 1 args

f1 :: (Val -> Val) -> PrimF
f1 f [obj] = return $ f obj
f1 _ args  = throwError $  NumArgs 1 args

predicate :: Predicate -> PrimF
predicate p = f1 (Boolean . p)

isSymbol :: Predicate
isSymbol (Symbol _) = True
isSymbol _        = False

isString :: Predicate
isString (String _) = True
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
isNumber _           = False

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
numericBinop _ args@[] = throwError $ NumArgs 1 args
numericBinop op args = mapM unpackNum args >>= return . Number . foldl1' op

unpackNum :: Val -> ThrowsErr Integer
unpackNum (Number n) = return n
unpackNum v          = throwError $ TypeMismatch "number" v

symbolToString' :: Val -> ThrowsErr Val
symbolToString' (Symbol a) = return $ String a
symbolToString' v        = throwError $ TypeMismatch "symbol" v

symbolToString :: Val -> Val
symbolToString (Symbol a) = String a
symbolToString _        = Boolean False -- FIX: raise exception

stringToSymbol' :: Val -> ThrowsErr Val
stringToSymbol' (String s) = return $ Symbol s
stringToSymbol' v          = throwError $ TypeMismatch "string" v

stringToSymbol :: Val -> Val
stringToSymbol (String s) = Symbol s
stringToSymbol _          = Boolean False -- FIX: raise exception

unwordsList :: [Val] -> String
unwordsList = unwords . map showVal

-- read+eval
r_e :: String -> ThrowsErr Val
r_e input = case parse parseExpr "schemey" input of
  Left err -> throwError $ Parser err
  Right val -> eval val

prVal :: (Show a1, Show a) => Either a a1 -> IO ()
prVal (Left err) = putStrLn $ show err
prVal (Right v)  = putStrLn $ show v

-- read+eval+print
r_e_p :: IO Bool
r_e_p = do
  putStr "schemey> " >> hFlush stdout
  end <- isEOF
  if end
     then putStrLn "" >> return True
     else do
       expr <- getLine
       if null expr
          then return False
          else prVal (r_e expr) >> return False

-- read+eval+print+loop
repl :: IO ()
repl = do
  quit <- r_e_p `catch` onUserInterrupt
  if quit then return () else repl

onUserInterrupt :: AsyncException -> IO Bool
onUserInterrupt UserInterrupt = putStrLn "\nquitting..." >> return True
onUserInterrupt e = throw e

usage :: IO ()
usage = putStrLn $ "usage: schemey [scheme-expression]"

main :: IO ()
main = do
  args <- getArgs
  case args of
    []     -> repl
    [expr] -> prVal $ r_e expr
    _      -> usage
