
main = do
  line <- getLine
  if null line 
    then return ()
    else do
      putStrLn $ reverseWords line
      main

reverseWords = unwords . map reverse . words
