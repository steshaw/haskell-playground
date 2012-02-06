-- Counting the lines in a file
import System (getArgs)
import IterateeM
import IterateeM.Extra

-- Iteratee-based solution. It seems faster than lazy IO
main :: IO ()
main = do
  [name] <- getArgs
  counter <- run =<< (enum_file name count_nl)
  print counter
