import Data.List (sort)
-- import Control.Arrow ((>>>))

main = readFile "poem" >>= putStr . process

-- Could import (>>>) from Control.Arrow but type looks weird. Sticking with something known.
(>>>) = flip (.)

process = lines >>> sort >>> unlines
