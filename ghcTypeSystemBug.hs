--
-- See http://yinwang0.wordpress.com/2012/03/05/ghc-type/
--

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ImpredicativeTypes #-}

gen :: [forall a. a -> a]
gen = [id]

-- broken
-- test1 = head gen 1

test2 = let hg = head gen in hg 1
