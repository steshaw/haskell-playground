module Main where

import Units
import Test.QuickCheck.Batch

options = TestOptions {
  no_of_tests = 100,
  length_of_tests = 1,
  debug_tests = False
}

main = do
  runTests "units" options 
    [run prop_eg1
    ,run prop_eg2
    ,run prop_eg3
    ,run prop_eg4
    ,run prop_eg5
    ,run prop_eg6
    ,run prop_eg7
    ,run prop_eg8
    ,run prop_eg9
    ,run prop_eg10
    ,run prop_eg11
    ,run prop_eg12
    ,run prop_eg13
    ]
