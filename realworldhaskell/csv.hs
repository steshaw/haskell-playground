module Main where

import Text.ParserCombinators.Parsec
import Text.Regex
import System.IO
import System.Environment

csvFile :: GenParser Char st [[String]]
csvFile = line `endBy` eol

line :: GenParser Char st [String]
line = cell `sepBy` (char ',')

cell :: GenParser Char st String
cell = quotedCell <|> regularCell

regularCell :: GenParser Char st String
regularCell = many (noneOf [',', '\n', '\r'])

quotedCell :: GenParser Char st String
quotedCell = char '"' >> many quotedChar >>= \r -> (char '"' <?> "end-double-quote") >> return r

quotedChar :: GenParser Char st Char
quotedChar = noneOf ['"'] <|> quotedDoubleQuote

quotedDoubleQuote :: GenParser Char st Char
quotedDoubleQuote = try (string (replicate 2 '"') >> return '"') <?> "quoted-double-quote"

eol :: GenParser Char st ()
eol = try (string "\r\n" >> return ())
  <|> (string "\n" >> return ())
  <|> (string "\r" >> return ())
  <?> "end of line"

parseCsv :: String -> Either ParseError [[String]]
parseCsv input = parse csvFile "(stdin)" input

main :: IO ()
main = do
  c <- getContents
  progName <- getProgName
  case parseCsv c of
    Right r -> mapM_ print r
    Left  e -> hPutStrLn stderr $ progName ++ ": Error parsing input:\n" ++ (indent (show e))
  where
    indent s = subRegex (mkRegex "^") s "  "
