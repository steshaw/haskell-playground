
import Data.List(sort)

main = readFile "poem" >>= putStr . process

process = unlines . sort . lines
