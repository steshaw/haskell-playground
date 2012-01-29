
-- http://code.google.com/p/omega/wiki/Epiphany

fak n = if n == 1 then 1 else n * fak (n - 1)

-- Cannot tie the recursive knot in Haskell due to infinite type (of f).
-- fakr f n = if n == 1 then 1 else n * f f (n - 1)
