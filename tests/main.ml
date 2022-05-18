open OUnit2
open RNAfoldml

(** Testing Doc: *)

(* ------------ Helper functions used in testing. ------------ *)

(** [string_of_lst printr l] is a string representation of [l] where
    each element, [e], is represented by [printr e]. *)
let string_of_list (to_string : 'a -> string) = function
  | [] -> "[]"
  | h :: t ->
      "["
      ^ List.fold_left
          (fun acc i -> acc ^ "; " ^ to_string i)
          (to_string h) t
      ^ "]"

(** [string_of_array printr a] is a string representation of [a] where
    each element, [e], is represented by [printr e]. *)
let string_of_array (to_string : 'a -> string) (a : 'a array) =
  a |> Array.to_list |> string_of_list to_string

(** [rna_test rna name seq] is a list of Ounit tests checking that
    [input.name = name] and [input.seq = expected_seq]. *)
let rna_test
    (input : Rna.t)
    (expected_name : string)
    (expected_seq : string) : test list =
  [
    ( input.name ^ " has name " ^ expected_name >:: fun _ ->
      assert_equal expected_name input.name );
    ( input.name ^ " has correct sequence" >:: fun _ ->
      assert_equal expected_seq input.seq );
  ]

(** [simple_pknot_test name input is_simp_pknot] is an Ounit test named
    [name] checking that if [is_simp_pknot] is true, then [input] is a
    simple pseudoknot and that [input] is not a simple pseudoknot
    otherwise. *)
let simple_pknot_test
    (name : string)
    (input : int array)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal
    (Secondary.is_simple_pknot input)
    expected_output ~printer:string_of_bool

(** [pknot_test name input is_pknot] is an Ounit test named [name]
    checking that if [is_pknot] is true, then [input] is a pseudoknot
    and that [input] is not a pseudoknot otherwise. *)
let pknot_test
    (name : string)
    (input : int array)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal
    (Secondary.is_pknot input)
    expected_output ~printer:string_of_bool

(** [compare_tests prefix s1 s2 d sim] is a list of Ounit tests checking
    that [s1] and [s2] have distance [d] and similarity [sim],
    respectively. *)
let compare_tests ?(prefix = "") struct1 struct2 exp_dist exp_sim =
  [
    ( prefix
    ^ Secondary.get_name struct1
    ^ " should have distance " ^ string_of_int exp_dist ^ " with "
    ^ Secondary.get_name struct2
    >:: fun _ ->
      assert_equal exp_dist
        (Secondary.distance struct1 struct2)
        ~printer:string_of_int );
    ( prefix
    ^ Secondary.get_name struct1
    ^ " should have similarity " ^ string_of_float exp_sim ^ " with "
    ^ Secondary.get_name struct2
    >:: fun _ ->
      assert_equal exp_sim
        (Secondary.similarity struct1 struct2)
        ~printer:string_of_float );
  ]

(** [secondary_print_load_tests sec] is an Ounit test case checking that
    the secondary structure of [sec] is the same after printing to a
    file and loading from the file. Tests printing to .ct and .dot
    formats unless [sec] is a pseudoknot in which case, only prints to
    .ct. *)
let secondary_print_load_tests (sec : Secondary.t) =
  let sec_name = Secondary.get_name sec in
  Secondary_print.to_ct ("test_output/" ^ sec_name ^ ".ct") sec;
  let ct_sec =
    Secondary_load.from_ct ("test_output/" ^ sec_name ^ ".ct")
  in
  compare_tests ~prefix:"print then load ct:" sec ct_sec 0 1.
  @
  (* Can only test with dot if sec is not a pseudoknot. *)
  if sec |> Secondary.get_pairs |> Secondary.is_pknot then []
  else (
    Secondary_print.to_dot ("test_output/" ^ sec_name ^ ".dot") sec;
    let dot_sec =
      Secondary_load.from_dot ("test_output/" ^ sec_name ^ ".dot")
    in
    compare_tests ~prefix:"print then load dot:" sec dot_sec 0 1.
    @ compare_tests ~prefix:"print then load ct vs dot:" ct_sec dot_sec
        0 1.)

let secondary_tests (sec : Secondary.t) =
  compare_tests sec sec 0 1. @ secondary_print_load_tests sec

(** [nussinov_tests rna exp_dot] is a list of Ounit test cases asserting
    that [Nussinov.predict rna] 1. is not a pseudoknot, 2. is not a
    simple pseudoknot and 3. has a dot string equal to [exp_dot]. *)
let nussinov_tests rna exp_dot =
  let nuss_sec = rna |> Nussinov.predict in
  secondary_tests nuss_sec
  @ [
      ( rna.name ^ " nussinov struct should not be a pseudoknot"
      >:: fun _ ->
        assert_equal false
          (nuss_sec |> Secondary.get_pairs |> Secondary.is_pknot)
          ~printer:string_of_bool );
      ( rna.name ^ " nussinov struct should not be a simple pseudoknot"
      >:: fun _ ->
        assert_equal false
          (nuss_sec |> Secondary.get_pairs |> Secondary.is_simple_pknot)
          ~printer:string_of_bool );
      ( rna.name ^ " nussinov struct has correct dot" >:: fun _ ->
        assert_equal exp_dot (nuss_sec |> Secondary_print.to_dot_string)
      );
    ]

(** [akutsu_test rna exp_pairs] is a list of Ounit test cases asserting
    that [Akutsu.predict] applied to an rna with sequence [rna] passes
    all of the tests in [secondary_tests] and has structure given by
    [exp_pairs]. *)
let akutsu_test
    (name : string)
    (input : string)
    (expected_output : int array) =
  let akuts_sec = Rna.from_string input name |> Akutsu.predict in
  secondary_tests akuts_sec
  @ [
      ( name >:: fun _ ->
        assert_equal
          (akuts_sec |> Secondary.get_pairs)
          expected_output
          ~printer:(string_of_array string_of_int) );
      ( name ^ " Akutsu preserves rna sequence" >:: fun _ ->
        assert_equal input (akuts_sec |> Secondary.get_seq) );
    ]

(* ------------ Rna values used in testing. ------------ *)

let fasta1 = Rna.from_fasta "test_data/test1.fasta"
let fasta2 = Rna.from_fasta "test_data/test2.fasta"
let rna1 = fasta1 |> List.hd |> Nussinov.predict
let rna2 = Rna.from_string "AGU" "small_rna"

let bigarr1 =
  let arr =
    Array.append
      [| 9; 12; 11; 18; -1; 16; 15; -1; 13; -1; 34; 22 |]
      [| 21; 33; 32; 31; 30; -1; 29; 28; 26; 25; 24; 23; 20; 7 |]
  in
  Array.append [| -1; -1; 6; -1; 5; 4; 2; 35; -1; 10 |] arr

let () =
  fasta1 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_ct "test_output/fasta1.ct"

let () =
  fasta1 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_dot "test_output/fasta1.dot"

let () =
  fasta2 |> List.hd |> Nussinov.predict
  |> Secondary_print.to_ct "test_output/fasta2.ct"

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

let exception_tests =
  [
    assert_raises (Invalid_argument "Invalid dot string") (fun _ ->
        Secondary_load.from_dot_string rna2 ")))");
    assert_raises (Invalid_argument "Unable to parse RNA sequence")
      (fun _ -> Secondary_load.from_dot_string rna2 "))))");
    assert_raises (Invalid_argument "Invalid char in dot string")
      (fun _ -> Secondary_load.from_dot_string rna2 "(.A");
    assert_raises Not_found (fun _ ->
        Secondary_load.from_dot "doesnotexist");
    assert_raises
      (Invalid_argument
         "Unable to load secondary structure from dot file: _config.yml")
      (fun _ -> Secondary_load.from_dot "_config.yml");
  ]

let nussinov_tests =
  List.flatten
    [
      nussinov_tests (fasta1 |> List.hd) "(((...)))";
      nussinov_tests
        (Rna.from_string "AAUUGGCC" "Simple_Test")
        "(())(())";
      nussinov_tests (fasta1 |> List.hd) "(((...)))";
      nussinov_tests (Rna.from_string "" "Empty") "";
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
  List.flatten
    [
      akutsu_test "akutsu_GGACCUUG" "GGACCUUG"
        [| 3; 4; -1; 0; 1; -1; -1; -1 |];
      akutsu_test "akutsu_AGAUC" "AGAUC" [| -1; 4; 3; 2; 1 |];
      akutsu_test "akutsu_AU" "AU" [| 1; 0 |];
      akutsu_test "akutsu_AGU" "AGU" [| 2; -1; 0 |];
      akutsu_test "akutsu_AGGU" "AGGU" [| 3; -1; -1; 0 |];
      akutsu_test "akutsu_AAAGGGUUCCC" "AAAGGGUUCCC"
        [| 6; -1; -1; 10; 9; 8; 0; -1; 5; 4; 3 |];
      akutsu_test "akutsu_AAACGGCUUUGAGCCUU" "AAACGGCUUUGAGCCUU"
        [| 8; 16; 15; -1; 14; 13; 12; 11; 0; -1; -1; 7; 6; 5; 4; 2; 1 |];
      akutsu_test "akutsu_AAGCUUGGCCAUG" "AAGCUUGGCCAUG"
        [| 5; 4; 3; 2; 1; 0; -1; 8; 7; 12; 11; 10; 9 |];
      akutsu_test "akutsu_AAGGCUUUGAGCCUU" "AAGGCUUUGAGCCUU"
        [| 6; 13; 12; 11; 10; 9; 0; -1; -1; 5; 4; 3; 2; 1; -1 |];
      akutsu_test "akutsu_AUU" "AUU" [| 1; 0; -1 |];
      akutsu_test "akutsu_GCC" "GCC" [| 1; 0; -1 |];
      akutsu_test "akutsu_AACUCUUCUAAGGUU" "AACUCUUCUAAGGUU"
        [| 8; 13; 12; -1; 11; 10; 9; -1; 0; 6; 5; 4; 2; 1; -1 |];
      akutsu_test "akutsu_AACUCUCUAGUGUU" "AACUCUCUAGUGUU"
        [| 7; 12; 11; -1; 9; 8; -1; 0; 5; 4; -1; 2; 1; -1 |];
      akutsu_test "akutsu_GGCUAUGUCA" "GGCUAUGUCA"
        [| -1; 2; 1; 9; 5; 4; 8; -1; 6; 3 |];
      akutsu_test "akutsu_GGCUAUGUUAUAUCUAUUGUGAUCUAGUAUCUAGCA"
        "GGCUAUGUUAUAUCUAUUGUGAUCUAGUAUCUAGCA" bigarr1;
    ]

let () = Secondary_vis.circle_graph "test_output/rna1" rna1

let tests =
  "test suite"
  >::: List.flatten
         [ rna_tests; nussinov_tests; pseudoknot_tests; akutsu_tests ]

let _ = run_test_tt_main tests