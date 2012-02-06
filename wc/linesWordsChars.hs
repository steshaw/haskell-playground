import System (getArgs)
import IterateeM
import IterateeM.Extra

-- Count the lines, words and characters in parallel
-- That is difficult to do with Lazy IO in constant space!

main :: IO ()
main = do
  [name] <- getArgs
  counter <- run =<<
    enum_file name (count_nl `enumPair` (runI =<< enum_words stream_count)
                             `enumPair` stream_count)
  print counter
