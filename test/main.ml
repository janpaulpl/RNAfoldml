open OUnit2
open RNAfoldml

(********************************************************************
   Here are some helper functions for your testing of set-like lists.
 ********************************************************************)


(********************************************************************
   End helper functions.
 ********************************************************************)


let suite =
  "test suite for A2"
  >::: List.flatten [  ]

let _ = run_test_tt_main suite
