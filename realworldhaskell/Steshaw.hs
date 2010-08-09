module Steshaw where

infixl 0 >>>
(>>>) = flip ($)

(>.>) = flip (.)
