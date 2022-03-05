type rna = {
  seq : string;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

let rep_ok str =
  match Str.search_forward (Str.regexp "\\([AGCU]+\\)") str 0 with
  | 0 -> str
  | _ -> failwith "RI"

let read_rna_fasta s =
  let x : rna list =
    [ { seq = ""; name = "sdas"; attributes = [ ("ad", "sds") ] } ]
  in
  if s = "" then x else x

let rna_from_string s name =
  let x : rna = { seq = ""; name; attributes = [ ("ad", "sds") ] } in
  if s = "" then x else x

let get_seq r = r.name
let get_info r = r.attributes
let get_name r = r.name
