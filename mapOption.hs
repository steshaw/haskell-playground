--
-- Inspired by discussion at https://groups.google.com/d/msg/pilud/auXjzeJN-vw/m-h1Hqz-H7gJ
--

(|>) = flip ($)

xs = [1..10] |> map (\n -> if even n then (Just n) else Nothing)

stripMaybes xs = do
  x <- xs
  case x of 
    (Just n) -> [n]
    _ -> []

stripMaybes' xs = do { x <- xs; case x of { (Just n) -> [n]; _ -> [] } }

a = stripMaybes xs
b = stripMaybes' xs

--
-- Once you have a List[Option[a]], F# has a nice List.choose function which can be used to
-- transform to a List[a].
--
-- In Haskell, one can use catMaybes (but it's not as general as List.choose).
--
