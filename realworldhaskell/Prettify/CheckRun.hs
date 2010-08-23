module Prettify.CheckRun where

import Prettify.Check
import Test.QuickCheck.Batch

options = TestOptions {
  no_of_tests = 1000,
  length_of_tests = 5,
  debug_tests = False
}

main = do
  runTests "simple" options [
    run prop_empty_id,
    run prop_char,
    run prop_text,
    run prop_line,
    run prop_double 
   ]

  runTests "complex" options [
    run prop_hcat,
    run prop_punctuate
   ]
