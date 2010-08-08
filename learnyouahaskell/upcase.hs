
import Data.Char(toUpper)

main = do
  getContents >>= (\contents -> putStr $ map toUpper contents)
