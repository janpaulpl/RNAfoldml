open OUnit2
open RNAfoldml

(* ------------ Helper functions used in testing. ------------ *)

let get_seq_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Rna.get_seq input)

let get_info_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Rna.get_info input)

let get_name_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Rna.get_name input)

(* ------------ Rna values used in testing. ------------ *)

let r1 =
  Rna.rna_from_string
    "AAAGCGGUUUGUUCCACUCGCGUUCAGUUAGCGUUUGCAGUUCUGGGCUC" "seq 1"

let r2 = Rna.rna_from_string "" "Empty Seq"

let r3 =
  match Rna.rna_from_fasta "test_data/test1.fasta" with
  | [ x ] -> x
  | _ -> failwith "test1 invalid"

let s1 = Rna.rna_from_string "AAACCCUUU" "Test Seq" |> Fold.predict

let rna_tests =
  [
    get_seq_test "Sequence from_string r1" r1
      "AAAGCGGUUUGUUCCACUCGCGUUCAGUUAGCGUUUGCAGUUCUGGGCUC";
    get_name_test "Name from_string r1" r1 "seq 1";
    get_info_test "No information from_string r1" r1 "";
    get_seq_test "Empty sequence has empty string" r2 "";
    get_name_test "Empty rna has name 'Empty Seq'" r2 "Empty Seq";
  ]

let fold_tests =
  [
    ( "Check Fold.predict retains sequence" >:: fun _ ->
      assert_equal "AAACCCUUU" (Fold.get_seq s1) );
    ( "Check Fold.predict retains info" >:: fun _ ->
      assert_equal "" (Fold.get_info s1) );
    ( "Check Fold.predict retains name" >:: fun _ ->
      assert_equal "Test Seq Secondary Structure" (Fold.get_name s1) );
    ( "Check Fold.predict has correct folding structure" >:: fun _ ->
      assert_equal "(((...)))" (Fold.to_dot_string s1) );
  ]

let tests = "test suite" >::: List.flatten [ rna_tests; fold_tests ]
let _ = run_test_tt_main tests