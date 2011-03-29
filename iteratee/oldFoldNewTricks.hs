data IterV el result
  = Done result (Stream el)
  | Cont (Stream el -> IterV el result)

-- Stream or Item/Position/Cursor?
--   Hmmm, but it does resemble a Cons with an extra case (End).
--     Hmmm, no actually, it's not a Cons at all because it's not a pair.
--     So, I guess it is more like an Item/Position/Cursor after all :).
data Stream a
  = Empty
  | Item a
  | End

enum :: IterV i a -> [i] -> IterV i a
enum i@(Done _ _) _ = i
enum (Cont k) (x:xs) = enum (k $ Item x) xs
enum i@(Cont _) [] = i

run :: (IterV i a) -> Maybe a
run (Done result _) = Just result
run (Cont k) = run' (k End)
  where
    run' (Done x _) = Just x
    run' _          = Nothing

sumS :: Stream Integer -> IterV Integer Integer
sumS = (sumS' 0)

sumS' :: Integer -> Stream Integer -> IterV Integer Integer
sumS' x s = case s of
  Empty  -> Done x End
  Item y -> Cont (sumS' (x + y))
  End    -> Done x End

go0 = run $ enum (Cont sumS) []
go1 = run $ enum (Cont sumS) [1]
go2 = run $ enum (Cont sumS) [1..2]
go3 = run $ enum (Cont sumS) [1..3]
go4 = run $ enum (Cont sumS) [1..4]
go5 = run $ enum (Cont $ sumS' 0) [1..5]
