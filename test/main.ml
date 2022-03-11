open OUnit2
open RNAfoldml
open Rna

(* rna values used in testing *)
let t1 =
  match rna_from_fasta "test_data/test1.fasta" with
  | [ x ] -> x
  | _ -> failwith "test1 invalid"

(* let [ t2; t3; t4 ] = match rna_from_fasta "test_data/test2.fasta"
   with | [ a; b; c; d ] -> [ a; b; c; d ] | _ -> failwith "test2
   invalid" *)

let broken_fasta = rna_from_fasta "test_data/broken.fasta"

let get_seq_test
    (name : string)
    (input : rna)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_seq input)

let get_info_test
    (name : string)
    (input : rna)
    (expected_output : (string * string) list) : test =
  name >:: fun _ -> assert_equal expected_output (get_info input)

let get_name_test
    (name : string)
    (input : rna)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (get_name input)

let rna_tests =
  [
    get_seq_test "Single sequence FASTA" t1
      "AAAGCGGUUUGUUCCACUCGCGUUCAGUUAGCGUUUGCAGUUCUGGGCUC";
  ]

let tests = "test suite" >::: List.flatten [ rna_tests ]
let _ = run_test_tt_main tests