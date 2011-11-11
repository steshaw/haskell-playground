primes = sieve [2..]
         where
         sieve (p:x) = p : sieve [n | n <- x, n `mod` p > 0]
