type rna = {
  seq : string;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

(** [rep_ok r] is a valid RNA sequence. Raises an exception if it is not
    a valid RNA sequence. *)
let rep_ok r =
  match Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 with
  | 0 -> r
  | _ -> failwith "RI"

let rna_from_fasta s =
  let x : rna list =
    [
      rep_ok { seq = ""; name = "sdas"; attributes = [ ("ad", "sds") ] };
    ]
  in
  if s = "" then x else x

let rna_from_string s name =
  let x : rna = { seq = ""; name; attributes = [ ("ad", "sds") ] } in
  if s = "" then x else x

let get_seq r = r.seq
let get_info r = r.attributes
let get_name r = r.name

(** Temporary read from FASTA file function. Returns the FASTA as a
    string.*)
let read_lines file process =
  let in_ch = open_in file in
  let rec read_line () =
    let line = try input_line in_ch with End_of_file -> exit 0 in
    process line;
    read_line ()
  in
  read_line ()
