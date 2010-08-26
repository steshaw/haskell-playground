module Main where

import Units
import Test.QuickCheck.Batch

options = TestOptions {
  no_of_tests = 100,
  length_of_tests = 1,
  debug_tests = False
}

main = do
  runTests "" options 
    [run prop_eg1
    ,run prop_eg2
    ,run prop_eg3
    ,run prop_eg4
    ,run prop_eg5
    ]
