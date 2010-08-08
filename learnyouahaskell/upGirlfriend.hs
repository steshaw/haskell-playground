
import System.IO
import Data.Char

main = readFile "girlfriend.txt" >>= writeFile "girlfriend.up.txt" . map toUpper
