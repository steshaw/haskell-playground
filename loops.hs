--
-- From http://www.xoltar.org/old_site/2003//sep/09/haskellLoops.html
--

import Data.IORef

-- Define 'foreach'
foreach = flip mapM_

-- Define 'while'
while test action = do
  val <- test
  if val then do {action;while test action}
         else return ()

-- Some helpers for use with 'while':
incr ref = modifyIORef ref (+1)
test ref f = do { val <- readIORef ref; return (f val) }

-- Exercise them. Equivalent Python code:
-- for x in range(1,11): 
--     print x
-- i = 0
-- while i < 5:
--     print "Still running"
--     i += 1

main = do
  foreach [1..10] $ \x ->
    print x
  ref <- newIORef 0
  while (test ref (< 5)) $ do
    putStrLn "Still running!"
    incr ref
