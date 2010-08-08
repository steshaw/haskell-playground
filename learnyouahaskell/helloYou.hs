import IO

main = do
  putStr "Hello, what's your name? "
  hFlush stdout
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
