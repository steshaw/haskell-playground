
import System.IO

-- main1 = readFile "girlfriend.txt" >>= putStr

main = do
  withFile "girlfriend.txt" ReadMode $ \ handle -> do
    hSetBuffering handle $ BlockBuffering $ Just 2048
    contents <- hGetContents handle
    putStr contents

