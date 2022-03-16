open OUnit2
open RNAfoldml

(* ------------ Helper functions used in testing. ------------ *)

let get_seq_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output input.seq

let get_info_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output input.info

let get_name_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output input.name

(* ------------ Rna values used in testing. ------------ *)

let r1 =
  Rna.rna_from_string
    "AAAGCGGUUUGUUCCACUCGCGUUCAGUUAGCGUUUGCAGUUCUGGGCUC" "seq 1"

let r2 = Rna.rna_from_string "" "Empty Seq"

let r3 =
  match Rna.rna_from_fasta "/test_data/test1.fasta" with
  | [ x ] -> x
  | _ -> failwith "test1 invalid"

let s1 =
  Rna.rna_from_string "AAACCCUUU" "Test seq 1" |> Secondary.predict

let s2 =
  Rna.rna_from_string "AAAAUCUUU" "Test seq 2" |> Secondary.predict

let () = print_endline (Secondary_print.to_dot_string s1)

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
    ( "Check Secondary.predict retains sequence" >:: fun _ ->
      assert_equal "AAACCCUUU" (Secondary.get_seq s1) );
    ( "Check Secondary.predict retains info" >:: fun _ ->
      assert_equal "" (Secondary.get_info s1) );
    ( "Check Secondary.predict has correct folding structure"
    >:: fun _ ->
      assert_equal "(((...)))" (Secondary_print.to_dot_string s1) );
    ( "Check Secondary.predict has correct folding structure"
    >:: fun _ ->
      assert_equal "(((().)))" (Secondary_print.to_dot_string s2) );
  ]

let tests = "test suite" >::: List.flatten [ rna_tests; fold_tests ]
let _ = run_test_tt_main tests