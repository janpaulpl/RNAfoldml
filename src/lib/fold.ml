open Array

type rna_sec_str = {
  seq : string;
  pairs : int array;
  name : string;
  has_pseudoknot : bool;
  info : string;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. [r.pairs i] is the index of the predicted base pairing
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
    && not (Str.string_match (Str.regexp "\\([AGCU]+\\)") r.seq 0)
  then failwith "rna secondary structure invalidate rep invariant."
  else r

(** [valid_pairs s i j] is true if and only if s.[i] and s.[j] form one
    of the 4 combinations which form a valid RNA base pair*)
let valid_pairs (seq : string) i j =
  match (seq.[i], seq.[j]) with
  | 'A', 'U' -> true
  | 'U', 'A' -> true
  | 'G', 'C' -> true
  | 'C', 'G' -> true
  | 'U', 'G' -> true
  | 'G', 'U' -> true
  | _ -> false

(** [get_pairs seq start fin] is the largest list of tuples (a,b) such
    that [valid_pairs a b] is true*)
let rec get_pairs (seq : string) (start : int) (fin : int) =
  if start >= fin then []
  else if valid_pairs seq start fin then
    (start, fin) :: get_pairs seq (start + 1) (fin - 1)
  else find_max seq start fin (fin - 1) []

(** [split seq start fin k]* is the list of tuples obtained by applying
    get_pairs to the strings obtained by chopping seq at position [k] *)
and split seq start fin k =
  get_pairs seq start k @ get_pairs seq (k + 1) fin

(** [find_max seq start fin k] is the largest list obtained by [split]
    after chopping seq at all of the possible k values*)
and find_max seq start fin k prev_max =
  let l = split seq start fin k in
  if k = start then
    if List.length prev_max > List.length l then prev_max else l
  else if List.length prev_max > List.length l then
    find_max seq start fin (k - 1) prev_max
  else find_max seq start fin (k - 1) l

(** [assoc_to_array seq pairs] is the [Array.t] in which for each entry
    [(i,j)] in [pairs], the entry at position [i] is [j] and the entry
    at position [j] is i. *)
let assoc_to_array seq (pairs : (int * int) list) =
  let arr = Array.make (String.length seq) 0 in
  List.iter
    (fun (a, b) ->
      Array.set arr a b;
      Array.set arr b a)
    pairs;
  arr

(** [nussinov r] is the secondary structure for [r] given by Nussinov's
    algorithm to maximize pairing. *)
let nussinov (r : Rna.t) =
  let pairs_assoc =
    get_pairs (Rna.get_seq r) 0 (String.length (Rna.get_seq r) - 1)
  in
  let pairs_array = assoc_to_array (Rna.get_seq r) pairs_assoc in
  {
    seq = Rna.get_seq r;
    pairs = pairs_array;
    name = Rna.get_name r ^ " Secondary Structure";
    has_pseudoknot = false;
    info = Rna.get_info r;
  }

let predict r = nussinov r |> rep_ok
let get_seq r = r.seq
let get_info r = r.info
let get_name r = r.name

let to_dot_string r =
  if r.has_pseudoknot then failwith "Not Yet Implemented"
  else
    r.pairs
    |> mapi (fun i j ->
           if i > j then "(" else if i < j then ")" else ".")
    |> fold_left ( ^ ) ""

let to_ct file r =
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
