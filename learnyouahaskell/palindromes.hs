
main = interact palindromes

(>>>) = flip (.)

palindromes :: String -> String
palindromes = lines >>> map (\xs -> if isPalindrome xs then "" else "not a ") >>> map (++ "palindrome") >>> unlines
  where isPalindrome xs = xs == reverse xs
