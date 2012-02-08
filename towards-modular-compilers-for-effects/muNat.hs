--
-- From http://lambda-the-ultimate.org/node/2687
--
-- See also "Data types à la carte" http://www.cs.ru.nl/~wouters/Publications/DataTypesALaCarte.pdf
--

newtype Mu f = In (f (Mu f))
data NatF b = Zero | Succ b
type Nat = Mu NatF
