--
-- Convert to Haskell from http://www.slideshare.net/paulk_asert/make-tests-groovy/158
--

import Control.Monad (forM_)

(|>) = flip ($)

test os mem disk = putStrLn $ "os: " ++ os ++ " mem: " ++ mem ++ " disk: " ++ disk

main = 
  [["MacOS", "Linux", "Vista"]
  ,["2G", "4G", "6G", "8G"]
  ,["250G", "350G", "500G"]
  ] |> sequence `forM_` \[os, mem, disk] -> test os mem disk
