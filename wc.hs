module Main where

count countType = interact wordCount
  where wordCount input = show (length (countType input)) ++ "\n"

countWords = count words
countLines = count lines
countChars = count id

main = countLines
