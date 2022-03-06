open OUnit2
open RNAfoldml
open Rna
open Out
open Fold

(* rna values used in testing *)
let rl1 = read_rna_fasta "test_data/test1.fasta"
let rl2 = read_rna_fasta "test_data/test1.fasta"
let rna_tests = []
let tests = "test suite" >::: List.flatten [ rna_tests ]
let _ = run_test_tt_main tests