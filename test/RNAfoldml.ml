open OUnit2
open RNAfoldml

let rep_test (input : rna) (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output (rep_ok input) ~printer:string_of_int

let rep_tests =
  [
    rep_test
      { seq = "AUGG"; name = "sdas"; attributes = [ ("ad", "sds") ] };
    0;
  ]

let tests = "test suite" >::: List.flatten [ rep_tests ]
let _ = run_test_tt_main tests