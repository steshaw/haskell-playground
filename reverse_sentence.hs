import Data.Char (isSpace)
import Data.Maybe (catMaybes)
import Data.Function (on)
import Data.List (groupBy)
import Control.Arrow ((&&&))

groupOnX :: Eq b => (a -> b) -> [a] -> [(b, [a])]
groupOnX f = tidy . groupOn . pairWith
  where
    tidy = map $ fst . head &&& map snd
    groupOn = groupBy ((==) `on` fst)
    pairWith = map (f &&& id)

reverseSentence :: String -> String
reverseSentence = concat . map reverseWord . groupOnX isSpace
  where
    reverseWord (spaces, s) = if spaces then s else reverse s

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
