
import Data.List(sort)

main = readFile "poem" >>= putStr . process

process t = unlines (sort (lines t))
