type rna_sec_str = {
  seq : string;
  pairs : (int * int) list;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. [r.pairs] contains predicted base pairs of the form
    [(i,j)] where [i] and [j] are indices of pairing nucleotides in seq.
    The string [r.name] represents the RNA sequence name. [r.attributes]
    represents the RNA sequence information.

    Representation invariant: [r.seq] only consists of characters 'A',
    'G', 'C', or 'U'. Any index [i] in any pair in [r.pairs] satisfies
    [1<=i<String.length r.seq]. Any such index [i] appears in at most 1
    pair in [r.pairs]. No index is paired with itself. *)

(** [valid_pair pairs s] is true if for all [(i,j)] in [pairs],
    [1<=i,j<String.length s], any such index [i] or [j] appear in at
    most 1 pair in [r.pairs], and [i <> j]. *)
let rec valid_pairs pairs seq_len =
  match pairs with
  | [] -> true
  | (i, j) :: t ->
      i <> j && 1 <= i && i < seq_len && 1 <= j && j < seq_len
      && List.for_all
           (function
             | a, b -> a <> i && b <> i && a <> j && b <> j)
           t
      && valid_pairs t seq_len

(** [rep_ok r] is the representation invariant checker. *)
let rep_ok r =
  if
    valid_pairs r.pairs (String.length r.seq)
    && Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 <> 0
  then failwith "Invalid Sequence in seq"
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
