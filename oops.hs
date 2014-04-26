module Main where

import qualified Prelude as P ((.))
import Prelude hiding ((.))

data D = D {s::String}

(∘) = (P..)
(.) = flip ($); d = D "oops"; f = filter (< 's')

a = length ∘ f ∘ s $ d
b = d.s.f.length
