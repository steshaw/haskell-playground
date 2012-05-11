{-# LANGUAGE ImplicitParams #-}

import Prelude hiding (show)
import qualified Prelude as P

data ShowDict alpha = ShowDict {
  namedShows :: alpha -> String -> String
}

show :: (?namedShow :: ShowDict alpha) => alpha -> String
show x = namedShows ?namedShow x ""

a = let ?namedShow = ShowDict { namedShows = \a s -> P.show a ++ s }
    in show 9
