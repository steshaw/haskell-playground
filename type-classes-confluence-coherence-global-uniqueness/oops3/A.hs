-- Code taken from http://stackoverflow.com/questions/12735274/breaking-data-set-integrity-without-generalizednewtypederiving/12744568#12744568
-- Discussion on haskell-cafe: http://thread.gmane.org/gmane.comp.lang.haskell.cafe/100870
--                             http://www.haskell.org/pipermail/haskell-cafe/2012-October/103984.html
-- Modified to remove orphan instances by rwbarton

module A where

data U = X | Y deriving (Eq, Ord, Show)
data T u b c = T u b c deriving (Eq, Show)
