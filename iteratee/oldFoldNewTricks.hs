module Iteratees where

import Prelude hiding (drop, length)

data IterV el result
  = Done result (Stream el)
  | Cont (Stream el -> IterV el result)

-- Stream or Item/Position/Cursor?
--   Hmmm, but it does resemble a Cons with an extra case (End).
--     Hmmm, no actually, it's not a Cons at all because it's not a pair.
--     So, I guess it is more like an Item/Position/Cursor after all :).
data Stream a
  = Empty
  | El a
  | End
  deriving (Show)

enum :: IterV el a -> [el] -> IterV el a
enum i@(Done _ _) _  = i
enum (Cont k) (x:xs) = enum (k $ El x) xs
enum i@(Cont _) []   = i

run :: (IterV el a) -> Maybe (a, Stream el)
run (Done result s) = Just (result, s)
run (Cont k) = run' (k End)
  where
    run' (Done x s) = Just (x, s)
    run' _          = Nothing

{-
remainderAsList :: IterV el a -> Maybe [el]
remainderAsList (Done _ rs) = Just $ foo rs
remainderAsList (Cont k) = Nothing
-}

head :: IterV el (Maybe el)
head = Cont step
  where
    step (El el) = Done (Just el) Empty
    step Empty   = Cont step
    step End     = Done Nothing End

peek :: IterV el (Maybe el)
peek = Cont step
  where
    step s@(El el) = Done (Just el) s
    step Empty     = Cont step
    step End       = Done Nothing End

drop :: Int -> IterV el ()
drop 0 = Done () Empty
drop n = Cont step
  where
    step (El _) = drop $ n - 1
    step Empty  = Cont step
    step End    = Done () End

length :: IterV el Int
length = Cont $ step 0
  where
    step :: Int -> Stream el -> IterV el Int
    step acc Empty  = Cont $ step acc
    step acc (El _) = Cont $ step $ acc + 1
    step acc End    = Done acc End

sumS :: Stream Integer -> IterV Integer Integer
sumS = step 0
  where
    step :: Integer -> Stream Integer -> IterV Integer Integer
    step x s = case s of
      Empty  -> Done x End
      El y -> Cont (step (x + y))
      End    -> Done x End

go0 = run $ enum (Cont sumS) []
go1 = run $ enum (Cont sumS) [1]
go2 = run $ enum (Cont sumS) [1..2]
go3 = run $ enum (Cont sumS) [1..3]
go4 = run $ enum (Cont sumS) [1..4]
go5 = run $ enum (Cont sumS) [1..5]
