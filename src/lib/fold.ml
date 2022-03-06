type rna_sec_str = {
  seq : string;
  pairs : (int * int) list;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: For secondary struct [r], the string [r.seq]
    represents a valid RNA sequence.

    Representation invariant: [r.seq] only contains characters *)

(** [rep_ok r] is the representation invariant checker. *)
let rep_ok r =
  if Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 <> 0 then
    failwith "Invalid Sequence in seq"
  else r

let temp =
  {
    seq = "temp";
    name = "temp";
    attributes = [ ("temp", "temp") ];
    pairs = [];
  }

(** [nussinov r] is the secondary structure for [r] given by Nussinov's
    algorithm to maximize pairing. *)
let nussinov (r : Rna.rna) = if r = r then temp else temp

let get_sec_str (rl : Rna.rna list) = List.map nussinov rl
