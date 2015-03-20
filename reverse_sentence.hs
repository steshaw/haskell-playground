import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (groupBy)
import Control.Arrow

dup :: a -> (a, a)
dup a = (a, a)

groupBySpace :: String -> [(Bool, String)]
groupBySpace xs = map (fst . head &&& map snd) $ groupBy ((==) `on` fst) $ map (first isSpace . dup) xs

reverseWord :: (Bool, String) -> String
reverseWord (True, s)  = s
reverseWord (False, s) = reverse s

reverseSentence :: String -> String
reverseSentence = concat . map reverseWord . groupBySpace

tests :: [(String, String)]
tests =
  [ (reverseSentence "", "")
  , (reverseSentence " ", " ")
  , (reverseSentence " \t\n ", " \t\n ")
  , (reverseSentence "a", "a")
  , (reverseSentence "ah", "ha")
  , (reverseSentence "dog", "god")
  , (reverseSentence "The cat and the dog", "ehT tac dna eht god")
  , (reverseSentence "The  cat and the dog ", "ehT  tac dna eht god ")
  , (reverseSentence "  \t\n The  cat\nand   the\tdog ", "  \t\n ehT  tac\ndna   eht\tgod ")
  ]

interrogateTest :: (String, String) -> Maybe String
interrogateTest (actual, expected) =
  if actual /= expected 
  then Just $ show actual ++ " != " ++ show expected
  else Nothing

runTests :: IO ()
runTests = do 
  mapM_ c results
  putStrLn $ "\n" ++ show (length results) ++ " tests, " ++ show numFails ++ " failures"
    where
      results = map interrogateTest tests
      c Nothing         = putStr "."
      c (Just errorMsg) = putStrLn $ "\nError: " ++ errorMsg
      numFails = length $ catMaybes results
