
-- See http://en.wikibooks.org/wiki/Haskell/Understanding_arrows

import Control.Arrow

addA f g = f &&& g >>> arr (\ (y,z) -> y + z)

clone a = (a,a)
