type rna = {
  seq : string;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

(** [rep_ok r] is the representation invariant checker. *)
let rep_ok r =
  match Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 with
  | 0 -> r
  | _ -> failwith "RI"

let read_rna_fasta s =
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
