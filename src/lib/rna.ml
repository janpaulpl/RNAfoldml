type rna = {
  seq : string;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. The string [r.name] represents the RNA sequence name. The
    string [r.attributes] represents the RNA sequence information.

    Representation invariant: [r.seq] only consists of characters 'A',
    'G', 'C', or 'U' *)

(** [rep_ok r] is [r] if the representation invariant holds on [r].
    Otherwise [rep_ok r] raises exception Failure("Invalidated rna
    ri."). *)
let rep_ok r =
  match Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 with
  | 0 -> r
  | _ -> failwith "Invalidated rna ri."

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
