module Steshaw where

infixl 0 >>>
(>>>) = flip ($)

(>.>) = flip (.)

--
-- Attempt to simulate something similar to Ruby's number literal syntax. e.g. 260_000
--
-- e.g. d[260,000]
--
d :: (Num a) => [a] -> a
d = foldl (\ acc i -> acc * 1000 + i) 0
