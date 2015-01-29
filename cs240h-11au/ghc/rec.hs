--
-- See https://groups.google.com/d/topic/stanford-11au-cs240h/QQBWg_RHnus/discussion
--

x0, y0, x1, y1, x2, y2 :: (Integer, Integer)

x0 = (1, snd y0)
y0 = (fst x0, 2)

fix :: (a -> a) -> a
fix f = let x = f x in x

(x1, y1) = fix $ \ ~(x', y') ->
  let x'' = (1, snd y')
      y'' = (fst x', 2)
  in (x'', y'')

fix' :: (a -> a) -> a
fix' f = f (fix' f)

(x2, y2) = fix' $ \ ~(x', y') ->
  let x'' = (1, snd y')
      y'' = (fst x', 2)
  in (x'', y'')

fact :: Integer -> Integer
fact = \n -> if n == 0 then 1 else n * fact (n - 1)

ffact :: (Integer -> Integer) -> Integer -> Integer
ffact f = \n -> if n == 0 then 1 else n * f (n - 1)

fact' :: Integer -> Integer
fact' = fix ffact

--
-- Y-combinator in Haskell using iso-recursive types.
--
-- See https://groups.google.com/d/topic/stanford-11au-cs240h/gRjdqUjD8_I/discussion
--
newtype SelfApply t = SelfApply { selfApply :: SelfApply t -> t }

y :: (t -> t) -> t
y f = selfApply term term
  where term = (SelfApply $ \x -> f (selfApply x x))

fact'' :: Integer -> Integer
fact'' = y ffact
