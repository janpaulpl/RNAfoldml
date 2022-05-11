(* *)
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
  fasta1 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_ct "test_output/fasta1.ct"

let () =
  fasta1 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_dot "test_output/fasta1.dot"

let () =
  fasta2 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_ct "test_output/fasta2.ct"

let fastanuss2 = Nussinov.predict (List.hd fasta2)

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

let algo_test name algo rna exp_dot =
  name >:: fun _ ->
  assert_equal (rna |> algo |> Secondary_print.to_dot_string) exp_dot

let nussinov_tests =
  [
    algo_test "Check Nussinov to_dot fasta1" Nussinov.predict
      (fasta1 |> List.hd) "(((...)))";
    algo_test "Check Nussinov to_dot AAUUGGCC" Nussinov.predict
      (Rna.from_string "AAUUGGCC" "Simple_Test")
      "(())(())";
    algo_test "Check Nussinov to_dot fasta1" Nussinov.predict
      (fasta1 |> List.hd) "(((...)))";
    algo_test "Check Nussinov to_dot Empty Seq" Nussinov.predict
      (Rna.from_string "" "Empty")
      "";
  ]

let pseudoknot_tests =
  [
    ( "Has simple pseudoknot example 1" >:: fun _ ->
      assert_equal
        (Secondary.is_simple_pknot
           [| 6; 4; -1; -1; 1; 8; 0; -1; 5; -1 |])
        true );
    ( "Has simple pseudoknot example 2" >:: fun _ ->
      assert_equal
        (Secondary.is_simple_pknot
           [| 9; -1; 7; -1; 6; -1; 4; 2; 13; 0; 12; -1; 10; 8; -1 |])
        true );
    ( "Has simple pseudoknot example 3" >:: fun _ ->
      assert_equal
        (Secondary.is_simple_pknot [| 5; 2; 1; -1; -1; 0 |])
        false );
    ( "Has simple pseudoknot example 4" >:: fun _ ->
      assert_equal
        (Secondary.is_simple_pknot
           [| 4; 5; -1; -1; 0; 1; 8; -1; 6; -1 |])
        false );
    ( "Has simple pseudoknot example 5" >:: fun _ ->
      assert_equal
        (Secondary.is_simple_pknot
           [| 5; 6; 8; -1; 11; 0; 1; 9; 2; 7; -1; 4 |])
        false );
    ( "Has pseudoknot example 1" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 7; 4; -1; -1; 1; 6; 5; 0 |])
        false );
    ( "Has pseudoknot example 2" >:: fun _ ->
      assert_equal (Secondary.is_pknot [| 4; -1; 5; 6; 0; 2; 3 |]) true
    );
    ( "Has pseudoknot example 3" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 3; 2; 1; 0; 6; 7; 4; 5 |])
        true );
    ( "Has pseudoknot example 4" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 1; 0; 3; 2; 7; -1; -1; 4 |])
        false );
    ( "Has pseudoknot example 5" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 5; 4; -1; -1; 1; 0; 7; 6 |])
        false );
    ( "Has pseudoknot example 6" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 3; -1; 6; 0; 5; 4; 2; -1 |])
        true );
    ( "Has pseudoknot example 7" >:: fun _ ->
      assert_equal
        (Secondary.is_pknot [| 5; 4; 6; 7; 1; 0; 2; 3; 9; 8 |])
        true );
    ( "Has pseudoknot example 8" >:: fun _ ->
      assert_equal (Secondary.is_pknot [| 1; 0; 3; 2; 5; 4 |]) false );
  ]

let akutsu_tests =
  [
    ( "Check Akutsu to_dot fasta1" >:: fun _ ->
      assert_equal
        (fasta1 |> List.hd |> Akutsu.predict
       |> Secondary_print.to_dot_string)
        "(((...)))" );
    ( "Check Akutsu to_dot empty" >:: fun _ ->
      assert_equal
        ("" |> Rna.from_string "" |> Akutsu.predict
       |> Secondary_print.to_dot_string)
        "" );
    ( "Check Akutsu GGACCUUG" >:: fun _ ->
      assert_equal
        (Rna.from_string "GGACCUUG" ""
        |> Akutsu.predict |> Secondary.get_pairs)
        [| 4; 3; 6; 1; 0; -1; 2; -1 |] );
    ( "Check Akutsu AGAUC" >:: fun _ ->
      assert_equal
        (Rna.from_string "AGAUC" ""
        |> Akutsu.predict |> Secondary.get_pairs)
        [| 3; 4; -1; 0; -1 |] );
  ]

let tests =
  "test suite"
  >::: List.flatten [ rna_tests; nussinov_tests; pseudoknot_tests ]

let _ = run_test_tt_main tests