--
-- In:
--
-- http://channel9.msdn.com/Shows/Going+Deep/C9-Lectures-Dr-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-4-of-13
--
-- Erik Meijer confusingly explains that given:
--   f x = 4711
-- 
-- that 
--    f (True && bottom)
-- =  f (bottom)
-- = 4711
--
-- but it seems that Haskell evaluates from the outside i.e. True && bottom is not evaluated at all.
--

import Prelude hiding ((&&))

(&&) :: Bool -> Bool -> Bool
True && b = b
False && _ = False

f x = 4711

bottom = bottom

try n = putStrLn $ show $ f n

main = do 
  try (True && bottom)
  try (bottom && bottom)
  try (bottom)
  try (error "hah!")
