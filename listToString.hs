--
-- Example of outputing lists in a way similar to Perl/Ruby's xs.join(", ") or Scala's xs.mkString(", ").
--
-- The type signatures on listToStringPF and listToStringPL are only required when
-- you don't give the no-monomorphism-restriction to GHC.
--
-- i.e. you can comment out the type signatures and compile with:
--
--   $ ghc -fno-monomorphism-restriction --make listToString.hs 
--
-- or get REPL with
--
--   $ ghci -fno-monomorphism-restriction listtoString.hs
--
-- Kinda makes me prefer pointful form... Who'm I kidding - I love pointful form :). Might be a hard habit to kick ;).
--
module Main where

import Data.List
import Control.Monad

-- @index join -- tells me that join can be found in
-- 15:23 < lambdabot> Control.Monad, Control.Monad.Reader, Control.Monad.Writer, Control.Monad.State, Control.Monad.RWS,
--                    Control.Monad.Identity, Control.Monad.Cont, Control.Monad.Error, Control.Monad.List

-- pointful version
listToString xs = concat (intersperse ", " (map show (xs)))

-- pointfree version. Requires type annotation :(
listToStringPF :: (Show a) => [a] -> [Char]
listToStringPF = concat . intersperse ", " . map show

-- listToString as pointed out by @pl (pointless plugin at #haskell).
listToStringPL :: (Show a) => [a] -> [Char]
listToStringPL = join . intersperse ", " . map show

main = do
  -- just playing here with let verses where syntax.
  let xs = [1..10] in do
    putStrLn $ listToString xs
    putStrLn $ listToStringPF xs
    putStrLn $ listToStringPL xs
  do
    putStrLn $ listToString xs
    putStrLn $ listToStringPF xs
    putStrLn $ listToStringPL xs
  where xs = [10..15]
