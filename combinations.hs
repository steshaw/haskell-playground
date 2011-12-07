--
-- Convert to Haskell from http://www.slideshare.net/paulk_asert/make-tests-groovy/158
--

import Control.Monad (forM_)

os = ["MacOS", "Linux", "Vista"]
mem = ["2G", "4G", "6G", "8G"]
disk = ["250G", "350G", "500G"]

combinations = sequence [os, mem, disk]

test os mem disk = putStrLn $ "os: " ++ os ++ " mem: " ++ mem ++ " disk: " ++ disk

main = combinations `forM_`
  \[os, mem, disk] ->
    test os mem disk
