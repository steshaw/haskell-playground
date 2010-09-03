module Main where

import Text.ParserCombinators.Parsec
import System.IO

csvFile :: GenParser Char st [[String]]
csvFile = do
  result <- many line
  eof
  return result

line :: GenParser Char st [String]
line = do
  result <- cells
  eol
  return result

cells :: GenParser Char st [String]
cells = do
  first <- cellContent
  next <- remainingCells
  return $ first : next

remainingCells :: GenParser Char st [String]
remainingCells = 
  (char ',' >> cells) <|> return []

cellContent :: GenParser Char st String
cellContent = many (noneOf [',', '\n'])

eol :: GenParser Char st Char
eol = char '\n'

parseCsv :: String -> String -> Either ParseError [[String]]
parseCsv fileName input = parse csvFile fileName input

main :: IO ()
main = do
  c <- getContents
  case parseCsv "(stdin)" c of
    Right r -> mapM_ print r
    Left  e -> hPutStrLn stderr $ "Error parsing input:\n" ++ (show e)
