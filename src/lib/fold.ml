open Array

type rna_sec_str = {
  seq : string;
  pairs : int array;
  name : string;
  attributes : (string * string) list;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. [get r.pairs i] is the index of the predicted base pairing
    with index [i]. If no base pairing at [i], [get r.pairs i = 0]. The
    string [r.name] represents the RNA sequence name. [r.attributes]
    represents the RNA sequence information.

    Representation invariant: [r.seq] only consists of characters 'A',
    'G', 'C', or 'U'. Length of pairs is the lenght of [r.seq]. Pairs
    relation is symmetric, i.e. if [j = get r.pairs i] then
    [i = get r.pairs j]. Also [i <> get r.pairs i]. *)

(** [check p i j] is true iff [i] is not [j], [i,j] in
    [\[0..length pairs\]], [p.i = j] and [p.j = i]. *)
let check pairs i j =
  i <> j && 0 < j
  && j < length pairs
  && 0 < i
  && i < length pairs
  && i = get pairs j
  && j = get pairs i

(** [rep_ok r] is [r] if [r] satisfies the [rna_sec_str] representation
    invariant. Otherwise [rep_ok r] raises
    [Failure "rna secondary structure invalidate rep invariant."] *)
let rep_ok r =
  if
    length r.pairs = String.length r.seq
    && fold_left ( && ) true (mapi (check r.pairs) r.pairs)
    && Str.search_forward (Str.regexp "\\([AGCU]+\\)") r.seq 0 <> 0
  then failwith "rna secondary structure invalidate rep invariant."
  else r

let temporary =
  { seq = ""; pairs = make 1 1; name = ""; attributes = [ (" ", "") ] }

(** [nussinov r] is the secondary structure for [r] given by Nussinov's
    algorithm to maximize pairing. *)
let nussinov (r : Rna.rna) =
  if r = r then failwith "unimplemented" else rep_ok temporary

let get_sec_str (rl : Rna.rna list) = List.map nussinov rl

let write_ct file r =
  let oc = open_out file in

  (* [print_ct_line i j] prints line [i] to oc file in .oc format where
     [seq.[i]] is paired to [j]. *)
  let print_ct_line i j =
    Printf.fprintf oc "%i %c %i %i %i %i\n" i r.seq.[i] (i - 1) (i + 1)
      j i
  in

  Printf.fprintf oc "%i %s\n" (String.length r.seq) r.name;
  iteri print_ct_line r.pairs;
  close_out oc
