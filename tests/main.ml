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

let simple_pknot_test
    (name : string)
    (input : int array)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal (Secondary.is_simple_pknot input) expected_output

let pknot_test
    (name : string)
    (input : int array)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal (Secondary.is_pknot input) expected_output

let akutsu_test
    (name : string)
    (input : string)
    (expected_output : int array) =
  name >:: fun _ ->
  assert_equal
    (Rna.from_string input "" |> Akutsu.predict |> Secondary.get_pairs)
    expected_output
(* ------------ Rna values used in testing. ------------ *)

let fasta1 = Rna.from_fasta "test_data/test1.fasta"
let fasta2 = Rna.from_fasta "test_data/test2.fasta"

let bigarr1 =
  let arr =
    Array.append
      [| 9; 12; 11; 6; -1; 16; 15; 35; 30; -1; 23 |]
      [| 22; 21; 20; -1; 27; -1; 25; 29; 28; 18; 32; 31; 34; 33; 17 |]
  in
  Array.append [| 2; -1; 0; 4; 3; -1; 13; -1; -1; 10 |] arr

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
    simple_pknot_test "simple_pknot example 1" [||] false;
    simple_pknot_test "simple_pknot example 1"
      [| 6; 4; -1; -1; 1; 8; 0; -1; 5; -1 |]
      true;
    simple_pknot_test "simple_pknot example 2"
      [| 9; -1; 7; -1; 6; -1; 4; 2; 13; 0; 12; -1; 10; 8; -1 |]
      true;
    simple_pknot_test "simple_pknot example 3"
      [| 5; 2; 1; -1; -1; 0 |]
      false;
    simple_pknot_test "simple_pknot example 4"
      [| 4; 5; -1; -1; 0; 1; 8; -1; 6; -1 |]
      false;
    simple_pknot_test "simple_pknot example 5"
      [| 5; 6; 8; -1; 11; 0; 1; 9; 2; 7; -1; 4 |]
      false;
    simple_pknot_test "simple_pknot example 6"
      [| 3; 2; 1; 0; 8; 7; 9; 5; 4; 6; -1 |]
      false;
    simple_pknot_test "simple_pknot example 7"
      [| 4; 3; -1; 1; 0; -1; -1 |]
      true;
    simple_pknot_test "simple_pknot example 8"
      [| 8; 7; 6; 5; -1; 3; 2; 1; 0 |]
      false;
    simple_pknot_test "simple_pknot example 9"
      [| 8; 7; -1; -1; -1; 14; 9; 1; 0; 6; -1; -1; -1; -1; 5 |]
      true;
    simple_pknot_test "simple_pknot example 10"
      [| 8; 7; -1; -1; -1; 14; 9; 1; 0; 6; 11; 10; -1; -1; 5 |]
      false;
    pknot_test "pseudoknot example 1"
      [| 7; 4; -1; -1; 1; 6; 5; 0 |]
      false;
    pknot_test "pseudoknot example 2" [| 4; -1; 5; 6; 0; 2; 3 |] true;
    pknot_test "pseudoknot example 3" [| 3; 2; 1; 0; 6; 7; 4; 5 |] true;
    pknot_test "pseudoknot example 4"
      [| 1; 0; 3; 2; 7; -1; -1; 4 |]
      false;
    pknot_test "pseudoknot example 5"
      [| 5; 4; -1; -1; 1; 0; 7; 6 |]
      false;
    pknot_test "pseudoknot example 6"
      [| 3; -1; 6; 0; 5; 4; 2; -1 |]
      true;
    pknot_test "pseudoknot example 7"
      [| 5; 4; 6; 7; 1; 0; 2; 3; 9; 8 |]
      true;
    pknot_test "pseudoknot example 8" [| 1; 0; 3; 2; 5; 4 |] false;
    pknot_test "pseudoknot example 9"
      [| 4; -1; -1; 6; 0; -1; 3; 9; -1; 7 |]
      true;
    pknot_test "pseudoknot example 10"
      [| 4; 2; 1; 6; 0; -1; 3; 9; -1; 7 |]
      true;
    pknot_test "pseudoknot example 11"
      [| -1; -1; -1; -1; -1; -1; -1; 9; -1; 7 |]
      false;
  ]

let akutsu_tests =
  [
    akutsu_test "Check Akutsu GGACCUUG" "GGACCUUG"
      [| 3; 4; -1; 0; 1; -1; -1; -1 |];
    akutsu_test "Check Akutsu AGAUC" "AGAUC" [| -1; 4; 3; 2; 1 |];
    akutsu_test "Check Akutsu AU" "AU" [| 1; 0 |];
    akutsu_test "Check Akutsu AGU" "AGU" [| 2; -1; 0 |];
    akutsu_test "Check Akutsu AGGU" "AGGU" [| 3; -1; -1; 0 |];
    akutsu_test "Check Akutsu AAAGGGUUCCC" "AAAGGGUUCCC"
      [| 6; -1; -1; 10; 9; 8; 0; -1; 5; 4; 3 |];
    akutsu_test "Check Akutsu AAACGGCUUUGAGCCUU" "AAACGGCUUUGAGCCUU"
      [| 8; 16; 15; -1; 14; 13; 12; 11; 0; -1; -1; 7; 6; 5; 4; 2; 1 |];
    akutsu_test "Check Akutsu AAGCUUGGCCAUG" "AAGCUUGGCCAUG"
      [| 5; 4; 3; 2; 1; 0; 8; -1; 6; 12; 11; 10; 9 |];
    akutsu_test "Check Akutsu AAGGCUUUGAGCCUU" "AAGGCUUUGAGCCUU"
      [| 6; 13; 12; 11; 10; 9; 0; -1; -1; 5; 4; 3; 2; 1; -1 |];
    akutsu_test "Check Akutsu AUU" "AUU" [| 1; 0; -1 |];
    akutsu_test "Check Akutsu GCC" "GCC" [| 1; 0; -1 |];
    akutsu_test "Check Akutsu AACUCUUCUAAGGUU" "AACUCUUCUAAGGUU"
      [| 8; 13; 12; -1; 11; 10; 9; -1; 0; 6; 5; 4; 2; 1; -1 |];
    akutsu_test "Check Akutsu AACUCUCUAGUGUU" "AACUCUCUAGUGUU"
      [| 7; 12; 11; -1; 9; 8; -1; 0; 5; 4; -1; 2; 1; -1 |];
    akutsu_test "Check Akutsu GGCUAUGUCA" "GGCUAUGUCA"
      [| 2; -1; 0; 4; 3; 9; 8; -1; 6; 5 |];
    akutsu_test "Check Akutsu GGCUAUGUUAUAUCUAUUGUGAUCUAGUAUCUAGCA"
      "GGCUAUGUUAUAUCUAUUGUGAUCUAGUAUCUAGCA" bigarr1;
  ]

let tests =
  "test suite"
  >::: List.flatten
         [ rna_tests; nussinov_tests; pseudoknot_tests; akutsu_tests ]

let _ = run_test_tt_main tests