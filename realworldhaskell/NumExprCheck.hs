module Main where

import NumExpr
import Test.QuickCheck.Batch

options = TestOptions {
  no_of_tests = 100,
  length_of_tests = 1,
  debug_tests = False
}

main = do
  runTests "pretty" options 
    [run prop_pretty_eg1
    ,run prop_pretty_eg2
    ,run prop_pretty_eg3
    ,run prop_pretty_eg4
    ]

  runTests "rpn" options 
    [run prop_rpn_eg1
    ,run prop_rpn_eg2
    ,run prop_rpn_eg3
    ,run prop_rpn_eg4
    ]

  runTests "testExpr" options 
    [run prop_test_1
    ,run prop_test_2
    ,run prop_test_3
    ,run prop_test_4
    ,run prop_test_5
    ,run prop_test_6
    ]

  runTests "props" options
    [run prop_pretty_1
    ,run prop_pretty_2
    ]

  runTests "props" options
    [run prop_pretty_1
    ,run prop_pretty_2
    ]
