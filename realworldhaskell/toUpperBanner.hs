import Data.Char(toUpper)
import Steshaw

--main = interact $ lines >.> map upLine >.> unlines
 -- where upLine s = "In uppercase: " ++ map toUpper s

main = interact $ lines >.> map (map toUpper >.> ("In uppercase: " ++)) >.> unlines
