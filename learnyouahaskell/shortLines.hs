
-- main = getContents >>= (\contents -> putStr $ shortLinesOnly contents)
main = interact shortLinesOnly

(>>>) = flip (.)

shortLinesOnly :: String -> String
shortLinesOnly = lines >>> filter (length >>> (<10)) >>> unlines
