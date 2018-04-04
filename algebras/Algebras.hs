

data Initial alg = Init (alg (Initial alg))
newtype Final f = Final (f (Final f))

newtype I a = I (a (I a)) -- Initial
newtype F f = F (f (F f)) -- Final
