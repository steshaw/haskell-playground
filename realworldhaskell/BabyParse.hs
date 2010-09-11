{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module BabyParse where

import Control.Monad.Error
import Control.Monad.State
import qualified Data.ByteString.Char8 as B
import Data.Char

data ParseError
  = NumericOverflow
  | EndOfInput
  | Chatty String
    deriving (Eq, Ord, Show)

instance Error ParseError where
  noMsg = Chatty "oh noes!"
  strMsg = Chatty

newtype Parser a = Parser {
  getParser :: ErrorT ParseError (State B.ByteString) a
} deriving (Monad, MonadError ParseError)

liftP :: State B.ByteString a -> Parser a
liftP m = Parser (lift m)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = do
  s <- liftP get
  case B.uncons s of
    Nothing -> throwError EndOfInput
    Just (c, s)
      | p c       -> liftP (put s) >> return c
      | otherwise -> throwError (Chatty "satisfy failed :(")

optional :: Parser a -> Parser (Maybe a)
optional p = (liftM Just p) `catchError` \_ -> return Nothing

many :: Parser a -> Parser [a]
many p = do
  r <- optional p
  case r of
    Nothing -> return []
    Just a -> do 
      r <- many p
      return (a:r)

many1 p = do
  r <- p
  r2 <- many p
  return (r:r2)

letter = satisfy isLetter

optionalLetter = optional letter

optionalWord = many1 letter
word = many1 letter

asdf = "asdf"
numExpr = "2.3 + 5.1"

eg1a = runState (runErrorT (getParser $ letter)) (B.pack asdf)
eg2a = runState (runErrorT (getParser $ letter)) (B.pack numExpr)
eg3a = runState (runErrorT (getParser $ optionalLetter)) (B.pack asdf)
eg4a = runState (runErrorT (getParser $ optionalLetter)) (B.pack numExpr)

runParser :: Parser a -> B.ByteString -> Either ParseError (a, B.ByteString)
runParser p bs = case (runState . runErrorT . getParser) p bs of
  (Left err, _) -> Left err
  (Right r, bs) -> Right (r, bs)

eg1b = runParser letter (B.pack asdf)
eg2b = runParser letter (B.pack numExpr)
eg3b = runParser optionalLetter (B.pack asdf)
eg4b = runParser optionalLetter (B.pack numExpr)

eg5 = runParser optionalWord (B.pack "one two three")
eg6 = runParser optionalWord (B.pack numExpr)

eg7 = runParser word (B.pack "one two three")
eg8 = runParser word (B.pack numExpr)
