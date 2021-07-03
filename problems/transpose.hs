{-# LANGUAGE BlockArguments #-}

import Control.Monad (when)

input :: [String]
input = [
  "aA",
  "bB",
  "cC"
  ]

expected :: [String]
expected = [
  "abc",
  "ABC"
  ]

-- transpose?
mystery :: [String] -> [String]
mystery xs =
  let shortest = minimum (map length xs) in
  map (\i -> map (!! i) xs) [0 .. shortest - 1]

main = do
  let actual = mystery input
  if actual /= expected then do
    putStrLn $ "Expected: " <> show expected
    putStrLn $ "Actual  : " <> show actual
  else
    putStrLn "ok"
