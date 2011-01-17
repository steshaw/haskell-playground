import Steshaw hiding (epsilon)

{-
main = print . last . takeWhile (/= 1) . map (+1) . iterate (/2) $ 1
-}

epsilon = 1 $> iterate (/2) $> map (+1) $> takeWhile (/= 1) $> last $> (-) 1

main = print epsilon
