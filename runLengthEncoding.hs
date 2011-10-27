--
-- From http://www.scribd.com/doc/48773692/Ignite-SV-Haskell-is-Better
--

runLengthEncode :: Eq a => [a] -> [(a, Int)]
runLengthEncode xs =
  foldr nextGroup [] xs
    where
      nextGroup x []        = [(x, 1)]
      nextGroup x gl@((g, n):gs)
        | x == g            = (g, n+1):gs
        | otherwise         = (x, 1):gl

main =
  let
    foo = "aaaaaaaaaabbbbbbbbbccccccccccD"
    encoded = runLengthEncode foo
  in putStrLn (show encoded)
