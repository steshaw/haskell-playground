module Main where

import Text.ParserCombinators.Parsec
import System.IO

csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` (char ',')

cell :: GenParser Char st String
cell = many (noneOf [',', '\n', '\r'])

eol :: GenParser Char st ()
eol = try (string "\r\n" >> return ())
  <|> (string "\n" >> return ())
  <|> (string "\r" >> return ())

parseCsv :: String -> String -> Either ParseError [[String]]
parseCsv fileName input = parse csvFile fileName input

main :: IO ()
main = do
  c <- getContents
  case parseCsv "(stdin)" c of
    Right r -> mapM_ print r
    Left  e -> hPutStrLn stderr $ "Error parsing input:\n" ++ (show e)
