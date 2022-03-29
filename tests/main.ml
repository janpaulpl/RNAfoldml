open OUnit2
open RNAfoldml

(* ------------ Helper functions used in testing. ------------ *)

let get_seq_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output input.seq

let get_name_test
    (name : string)
    (input : Rna.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output input.name

(* ------------ Rna values used in testing. ------------ *)

let fasta1 = Rna.from_fasta "test_data/test1.fasta"
let fasta2 = Rna.from_fasta "test_data/test2.fasta"

let () =
  fasta1 |> List.hd |> Secondary.predict
  |> Secondary_print.to_ct "test_output/fasta1.ct"

let () =
  fasta1 |> List.hd |> Secondary.predict
  |> Secondary_print.to_dot "test_output/fasta1.dot"

let () =
  fasta2 |> List.hd |> Secondary.predict
  |> Secondary_print.to_ct "test_output/fasta2.ct"

let fastanuss2 = Secondary.predict (List.hd fasta2)

(* ------------ Tests ------------ *)

let rna_tests =
  [
    ( "Check Fasta1 to_fasta length" >:: fun _ ->
      assert_equal (List.length fasta1) 1 );
    ( "Check Fasta1 to_fasta name" >:: fun _ ->
      assert_equal (List.hd fasta1).name "AAA" );
    ( "Check Fasta1 to_fasta sequence" >:: fun _ ->
      assert_equal (List.hd fasta1).seq "AAACCCUUU" );
    ( "Check Fasta2 to_fasta length" >:: fun _ ->
      assert_equal (List.length fasta2) 4 );
    ( "Check Head Fasta2 to_fasta name" >:: fun _ ->
      assert_equal (List.hd fasta2).name "Sequence_1" );
    ( "Check Head Fasta2 to_fasta sequence" >:: fun _ ->
      assert_equal (List.hd fasta2).seq
        "AAAGCGGUUUGUUCCACUCGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGUUUGCAGCGUUCAGUUAGCGU"
    );
    ( "Check Empty to_fasta length" >:: fun _ ->
      assert_equal
        ("test_data/empty.fasta" |> Rna.from_fasta |> List.length)
        0 );
  ]

let fold_tests =
  [
    ( "Check Nussinov to_dot fasta1" >:: fun _ ->
      assert_equal
        (fasta1 |> List.hd |> Secondary.predict
       |> Secondary_print.to_dot_string)
        "(((...)))" );
    ( "Check Nussinov to_dot AAUUGGCC" >:: fun _ ->
      assert_equal
        (Rna.from_string "AAUUGGCC" "Simple_Test"
        |> Secondary.predict |> Secondary_print.to_dot_string)
        "(())(())" );
    ( "Check Nussinov to_dot Empty Seq" >:: fun _ ->
      assert_equal
        ~printer:(fun s -> s)
        (Rna.from_string "" "Empty"
        |> Secondary.predict |> Secondary_print.to_dot_string)
        "" );
  ]

let pseudoknot_tests =
  [
    ( "Simple pseudoknot example 1" >:: fun _ ->
      assert_equal
        (Secondary.check_is_simple_pknot "          "
           [| 6; 4; -1; -1; 1; 8; 0; -1; 5; -1 |]
           3 7)
        true );
    ( "Simple pseudoknot example 2" >:: fun _ ->
      assert_equal
        (Secondary.check_is_simple_pknot "               "
           [| 9; -1; 7; -1; 6; -1; 4; 2; 13; 0; 12; -1; 10; 8; -1 |]
           5 11)
        true );
    ( "Has simple pseudoknot example 1" >:: fun _ ->
      assert_equal
        (Secondary.check_has_simple_pknot "          "
           [| 6; 4; -1; -1; 1; 8; 0; -1; 5; -1 |])
        true );
    ( "Has simple pseudoknot example 2" >:: fun _ ->
      assert_equal
        (Secondary.check_has_simple_pknot "               "
           [| 9; -1; 7; -1; 6; -1; 4; 2; 13; 0; 12; -1; 10; 8; -1 |])
        true );
    ( "Has simple pseudoknot example 3" >:: fun _ ->
      assert_equal
        (Secondary.check_has_simple_pknot "      "
           [| 5; 2; 1; -1; -1; 0 |])
        false );
    ( "Has simple pseudoknot example 4" >:: fun _ ->
      assert_equal
        (Secondary.check_has_simple_pknot "          "
           [| 4; 5; -1; -1; 0; 1; 8; -1; 6; -1 |])
        false );
    ( "Has simple pseudoknot example 5" >:: fun _ ->
      assert_equal
        (Secondary.check_has_simple_pknot "           "
           [| 5; 6; 8; -1; 11; 0; 1; 9; 2; 7; -1; 4 |])
        false );
  ]

let tests =
  "test suite"
  >::: List.flatten [ rna_tests; fold_tests; pseudoknot_tests ]

let _ = run_test_tt_main tests