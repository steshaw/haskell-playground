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
-- I paused too quickly and Erik does go on to explain that f (bottom) = 4711 is just 4711 is more Haskellish.
-- However, then goes on to say that the order of evaluation does not matter in Haskell (when it seems that it
-- does).
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
