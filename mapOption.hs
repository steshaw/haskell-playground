--
-- Inspired by discussion at https://groups.google.com/d/msg/pilud/auXjzeJN-vw/m-h1Hqz-H7gJ
--
-- Mike Austin's code example looks like:
--
--  [1..10] map: |n| if n even then n
--

import Data.Maybe (mapMaybe)

(|>) = flip ($)

xs = [1..10]

ys = xs |> map (\n -> if even n then (Just n) else Nothing)

stripMaybes xs = do
  x <- xs
  case x of 
    (Just n) -> [n]
    _ -> []

stripMaybes' xs = do { x <- xs; case x of { (Just n) -> [n]; _ -> [] } }

stripA = stripMaybes ys
stripB = stripMaybes' ys

--
-- Once you have a List[Option[a]], F# has a nice List.choose function which can be used to
-- transform to a List[a].
--
-- In Haskell, one can use catMaybes (but it's not as general as List.choose).
--

a = xs |> filter even

{-
 -
 - but's let's say we do more then "return" n:
 -
 -   [1..10] map: |n| if n even then n*2
 -
 -}

b = xs |> mapMaybe (\ n -> if even n then Just (n * 2) else Nothing)

ifO p n = if p n then Just n else Nothing

a' = xs |> mapMaybe (ifO even)

b' = xs |> mapMaybe (\ n -> ifO even n |> fmap (* 2))

ifOm p n f = ifO p n |> fmap f

b'' = xs |> mapMaybe (\ n -> ifOm even n (* 2))
