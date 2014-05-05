module Main where

import qualified Prelude as P ((.))
import Prelude hiding ((.))
import Control.Lens

data D = D {s::String}

(∘) = (P..)
(.) = flip ($); d = D "oops"; f = filter (< 's')

a  = length ∘ f ∘ s $ d
b  = d.s.f.length
c  = d&s&f&length -- alternative using a utility from Control.Lens. Thanks to Javran Uptoiso https://twitter.com/JavranC.
c' = d & s & f & length -- a more readable alternative but still prefer the "OO" syntax.
