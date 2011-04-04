-- Haskell98!
-- Strictness is observable
-- With no seq, no unsafe operations

import Control.Exception

-- fs and fns are both essentially (const True) functions,
-- but differ in strictness
fs,fns :: Bool -> Bool

-- non-strict
fns x = True

-- strict
fs True = True
fs x    = True

handler :: SomeException -> IO ()
handler _ = print "strict"

test f = handle handler $
         if f (error "Bang!") then print "non-strict" else return ()

main_s = test fs
-- prints "strict"

main_ns = test fns
-- prints "non-strict"
