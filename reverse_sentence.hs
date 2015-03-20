import Data.Char (isSpace)
import Data.Maybe (catMaybes)

newtype Spaces = Spaces String
newtype Word = Word String

type S = Either Spaces Word

myWords :: String -> [S]
myWords [] = []
myWords s =
  let (e1, s1) = span isSpace s in
  let (e2, s2) = span (not . isSpace) s1 in Left (Spaces e1) : Right (Word e2) : myWords s2

reverseWord :: S -> String
reverseWord (Left (Spaces s)) = s
reverseWord (Right (Word s))  = reverse s

reverseSentence :: String -> String
reverseSentence = concat . map reverseWord . myWords

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
