import Data.Char

main = do
  putStrLn "What's your first name?"
  firstName <- getLine
  putStrLn "What's your last name?"
  lastName <- getLine
  let upFirstName = map toUpper firstName
      upLastName = map toUpper lastName
  putStrLn $ "hey " ++ upFirstName ++ " " ++ upLastName ++ ", how are you?"
