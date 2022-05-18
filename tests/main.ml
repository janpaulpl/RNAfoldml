open OUnit2
open RNAfoldml

(** Testing Doc: Every function except those in [secondary_vis.ml] is
    automatically tested. This is due to the fact that
    [secondary_vis.ml] generates a PDF file which can't be verified for
    correctness. All the test cases are developed using glass-box
    testing. Our suite proves system correctness when verified with the
    bisect tool; every file is at least 90% covered. More information
    can be found in [_coverage/index.html].*)

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

(** [nuss_pair_test name input expected_output] is a OUnit test which
    passes if and only if the Nussinov algorithm predicts the bindings
    which agree with [expected_output]*)
let nuss_pair_test
    (name : string)
    (input : string)
    (expected_output : int array) =
  name >:: fun _ ->
  assert_equal
    (Rna.from_string input name
    |> Nussinov.predict |> Secondary.get_pairs)
    expected_output

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

(** These arrays are for the purpose of testing Akutsu and Nussinov
    algorithmns. They are created like this because when the arrays get
    too long, OCaml autoformatting puts 1 number on each line. We wanted
    to avoid this because it artificially inflates lines, so we resorted
    to appending smaller arrays together/*)
let bigarr1 =
  let arr =
    Array.append
      [| 9; 12; 11; 18; -1; 16; 15; -1; 13; -1; 34; 22 |]
      [| 21; 33; 32; 31; 30; -1; 29; 28; 26; 25; 24; 23; 20; 7 |]
  in
  Array.append [| -1; -1; 6; -1; 5; 4; 2; 35; -1; 10 |] arr

let bigarr2 =
  Array.append
    [| 22; 21; -1; -1; 5; 4; 16; -1; -1; 15 |]
    [| 14; -1; -1; -1; 10; 9; 6; 20; 19; 18; 17; 1; 0 |]

let bigarr3 =
  Array.append
    [| 27; -1; 10; 4; 3; 9; 8; -1; 6; 5; 2; 26; -1; 21; 20 |]
    [| 19; -1; 18; 17; 15; 14; 13; 25; 24; 23; 22; 11; 0 |]

let bigarr4 =
  Array.append
    [| 3; 2; 1; 0; 23; 22; -1; -1; 20; -1; 15; 14; 13 |]
    [| 12; 11; 10; 19; 18; 17; 16; 8; -1; 5; 4 |]

let bigarr5 =
  let arr =
    Array.append
      [| 36; 35; -1; 5; -1; 3; 9; 8; 7; 6; 11; 10; 19; 18 |]
      [| -1; 16; 15; -1; 13; 12; -1; -1; 33; 31; 27; 26; 25 |]
  in
  Array.append arr [| 24; 30; -1; 28; 23; -1; 22; -1; 1; 0 |]

let bigarr6 =
  Array.append
    [| -1; -1; -1; 17; 16; 15; 10; 9; -1; 7; 6; 14; 13; 12; 11; 5 |]
    [| 4; 3; 27; -1; -1; -1; -1; 24; 23; 26; 25; 18 |]

let bigarr7 =
  Array.append
    [| 7; 19; 17; 15; 14; 13; 12; 0; -1; -1 |]
    [| -1; -1; 6; 5; 4; 3; -1; 2; -1; 1; -1 |]

let bigarr8 =
  Array.append
    [| 10; 9; -1; 6; 5; 4; 3; -1; -1; 1; 0; -1; 25 |]
    [| -1; -1; 23; 22; 18; 17; -1; 21; 20; 16; 15; -1; 12; -1 |]

let bigarr9 =
  Array.append
    [| -1; 4; -1; -1; 1; 23; 22; 11; 10; -1; 8; 7; 21; 14 |]
    [| 13; 20; 19; 18; 17; 16; 15; 12; 6; 5; 27; 26; 25; 24 |]

let bigarr10 =
  Array.append
    [| -1; 5; 4; -1; 2; 1; 8; -1; 6; 30; 29; 28; -1; 14; 13; 16; 15 |]
    [| -1; 22; 21; -1; 19; 18; 26; 25; 24; 23; -1; 11; 10; 9 |]

let bigarr11 =
  Array.append
    [| 29; 3; -1; 1; 7; -1; -1; 4; 9; 8; 28; 27; 26 |]
    [|
      -1; 24; 23; 22; -1; 21; 20; 19; 18; 16; 15; 14; -1; 12; 11; 10; 0;
    |]

let bigarr12 =
  Array.append
    [| 1; 0; -1; 4; 3; 27; -1; 25; 24; 13; -1; 12; 11; 9 |]
    [| -1; -1; 17; 16; -1; 22; 21; 20; 19; -1; 8; 7; -1; 5 |]

let bigarr13 =
  Array.append
    [| -1; -1; -1; 19; -1; -1; -1; 17; -1; 16; 13; 12; 11 |]
    [| 10; 15; 14; 9; 7; -1; 3; -1; -1 |]

let bigarr14 =
  Array.append
    [| 1; 0; -1; -1; -1; 18; 9; 8; 7; 6; 17; 16 |]
    [| 15; -1; -1; 12; 11; 10; 5; 21; -1; 19 |]

let bigarr15 =
  Array.append
    [| -1; 14; 9; 8; 7; 6; 5; 4; 3; 2; 13 |]
    [| 12; 11; 10; 1; -1; -1; -1; -1; -1; -1 |]

let bigarr16 =
  Array.append
    [| 26; -1; -1; 17; 16; 8; -1; -1; 5; 15; 14; 13; -1; 11; 10 |]
    [| 9; 4; 3; -1; 22; -1; -1; 19; -1; -1; -1; 0 |]

let bigarr17 =
  Array.append
    [| -1; 2; 1; 4; 3; 8; 7; 6; 5; 26; -1; 24; -1; -1 |]
    [| 21; 20; -1; 19; -1; 17; 15; 14; -1; -1; 11; -1; 9; -1 |]

let bigarr18 =
  Array.append
    [| 7; 3; -1; 1; 5; 4; -1; 0; 20; -1; 11; 10; 19; 14; 13; 18; 17 |]
    [| 16; 15; 12; 8; -1; -1; -1; 29; -1; 27; 26; -1; 24; 31; 30 |]

let bigarr19 =
  Array.append
    [| 6; 5; -1; 4; 3; 1; 0; 8; 7; -1; 26; -1; -1; 14; 13; 23; -1 |]
    [| -1; -1; -1; -1; -1; -1; 15; -1; -1; 10 |]

let bigarr20 =
  Array.append
    [| 6; 5; -1; 4; 3; 1; 0; 8; 7; -1; 27; 26; 17; -1; 15; 14; -1; 12 |]
    [| -1; 20; 19; 24; 23; 22; 21; -1; 11; 10 |]

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
    assert_raises Not_found (fun _ ->
        Secondary_load.from_ct "nonsense!");
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
  @ [
      nuss_pair_test "nuss_empty" "" [||];
      nuss_pair_test "nuss_GGACCCUAUGUUG" "GGACCCUAUGUUG"
        [| -1; 3; -1; 1; 12; 9; -1; 8; 7; 5; -1; -1; 4 |];
      nuss_pair_test "nuss_ACUAUGUUCAU" "ACUAUGUUCAU"
        [| 10; -1; 9; 4; 3; 8; -1; -1; 5; 2; 0 |];
      nuss_pair_test "nuss_AACUGUUCAUCAUUGUAUCAUGUACUAUGU"
        "AACUGUUCAUCAUUGUAUCAUGUACUAUGU" bigarr11;
      nuss_pair_test "nuss_AUCAUGUAUCUAUGUUAUUGAUCUAUUC"
        "AUCAUGUAUCUAUGUUAUUGAUCUAUUC" bigarr12;
      nuss_pair_test "nuss_UUGUGUUGUAGUACUAUCUAGU"
        "UUGUGUUGUAGUACUAUCUAGU" bigarr13;
      nuss_pair_test "nuss_AUUUUUUGCAGUAUCUACAGUC"
        "AUUUUUUGCAGUAUCUACAGUC" bigarr14;
      nuss_pair_test "nuss_UUUGGGCCCAGUACAGGGGGG"
        "UUUGGGCCCAGUACAGGGGGG" bigarr15;
      nuss_pair_test "nuss_GUUCUGUUCUACUGUAAGUAUUUUGGC"
        "GUUCUGUUCUACUGUAAGUAUUUUGGC" bigarr16;
      nuss_pair_test "nuss_CCGGCCUAGUAUCAUUGUGAAAAAAAAA"
        "CCGGCCUAGUAUCAUUGUGAAAAAAAAA" bigarr17;
      nuss_pair_test "nuss_AACUAUCUGUAUGUAGUACCCCCCCUAUUGUA"
        "AACUAUCUGUAUGUAGUACCCCCCCUAUUGUA" bigarr18;
      nuss_pair_test "nuss_ACUAUGUAUUGUUAUCGUGGGGGGGUC"
        "ACUAUGUAUUGUUAUCGUGGGGGGGUC" bigarr19;
      nuss_pair_test "nuss_ACUAUGUAUGUAGUUAUCUAUGUACUUA"
        "ACUAUGUAUGUAGUUAUCUAUGUACUUA" bigarr20;
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
      akutsu_test "akutsu_empty" "" [||];
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
      akutsu_test "akutsu_ACUGAUCUAGGGAUC" "ACUGAUCUAGGGAUC"
        [| -1; 3; -1; 1; 5; 4; 9; 8; 7; 6; 14; -1; 13; 12; 10 |];
      akutsu_test "akutsu_AUCUAUCUUCUUUCAGGGAUCAU"
        "AUCUAUCUUCUUUCAGGGAUCAU" bigarr2;
      akutsu_test "akustu_AUCAUCUUAGGGGGACUAUGUCAUAUCU"
        "AUCAUCUUAGGGGGACUAUGUCAUAUCU" bigarr3;
      akutsu_test "akutsu_GGCCAUCUGUACUAGUGUACCUAU"
        "GGCCAUCUGUACUAGUGUACCUAU" bigarr4;
      akutsu_test "akutsu_GUCACUGAUCAUGAUAUUUCCCUGGUACAUUCUAUAC"
        "GUCACUGAUCAUGAUAUUUCCCUGGUACAUUCUAUAC" bigarr5;
      akutsu_test "akutsu_AAACCCACUGUCAUGGGGGGGGGGCAUC"
        "AAACCCACUGUCAUGGGGGGGGGGCAUC" bigarr6;
      akutsu_test "akutsu_AAAACCCUUUGGGGGUCUAUC" "AAAACCCUUUGGGGGUCUAUC"
        bigarr7;
      akutsu_test "akutsu_CAUCAUGUUUGUGUGUGGCUAUCAUCU"
        "CAUCAUGUUUGUGUGUGGCUAUCAUCU" bigarr8;
      akutsu_test "akutsu_AACCUUGGUCACUAUGUAUACACACAUG"
        "AACCUUGGUCACUAUGUAUACACACAUG" bigarr9;
      akutsu_test "akutsu_UUGUCAGUCCUGUGCAUUUUCAACCGGUCAG"
        "UUGUCAGUCCUGUGCAUUUUCAACCGGUCAG" bigarr10;
    ]

let () = Secondary_vis.circle_graph "test_output/rna1" rna1

let tests =
  "test suite"
  >::: List.flatten
         [ rna_tests; nussinov_tests; pseudoknot_tests; akutsu_tests ]

let _ = run_test_tt_main tests