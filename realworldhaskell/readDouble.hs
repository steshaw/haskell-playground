
main = do
  putStrLn "Please enter a double: "
  input <- getLine
  let d = (read input) :: Double
  putStrLn $ "Twice " ++ show d ++ " is " ++ show (d * 2)
