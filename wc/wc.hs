import System (getArgs)

main :: IO ()
main = do
  [name] <- getArgs
  file <- readFile name
  print $ length $ lines file
