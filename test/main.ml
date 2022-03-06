open OUnit2
open RNAfoldml
open Rna
open Out
open Fold

(* rna values used in testing *)
let rl1 = rna_from_fasta "test_data/test1.fasta"
let rl2 = rna_from_fasta "test_data/test2.fasta"
let broken_fasta = rna_from_fasta "test_data/broken.fasta"

let rec tests f lst =
  match lst with
  | [] -> Not_found
  | h :: t ->
      f h;
      tests f t

let get_seq_test (input : rna) (expected_output : string) : test =
  "" >:: fun _ -> assert_equal expected_output (get_seq input)

let get_info_test
    (input : rna)
    (expected_output : (string * string) list) : test =
  "" >:: fun _ -> assert_equal expected_output (get_info input)

let get_name_test (input : rna) (expected_output : string) : test =
  "" >:: fun _ -> assert_equal expected_output (get_name input)

let rna_tests = [ get_seq_test rl1.rna "test" ]
let tests = "test suite" >::: List.flatten [ rna_tests ]
let _ = run_test_tt_main tests