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

exception Invalid_RI

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
    invariant. Otherwise [rep_ok r] raises [Invalid_RI] *)
let rep_ok r =
  if
    length r.pairs = String.length r.seq
    && fold_left ( && ) true (mapi (check r.pairs) r.pairs)
    && not (Str.string_match (Str.regexp "\\([AGCU]+\\)") r.seq 0)
  then raise Invalid_RI
  else r

(** [valid_pairs s i j] is true if and only if s.[i] and s.[j] form one
    of the 4 combinations which form a valid RNA base pair*)
let valid_pair (seq : string) i j =
  match (seq.[i], seq.[j]) with
  | 'A', 'U' -> true
  | 'U', 'A' -> true
  | 'G', 'C' -> true
  | 'C', 'G' -> true
  | 'U', 'G' -> true
  | 'G', 'U' -> true
  | _ -> false

(** [largest_lst l] is the list with maximum length *)
let rec largest_lst = function
  | h :: t ->
      let lt = largest_lst t in
      if List.length h > List.length lt then h else lt
  | [] -> []

(** [max_pairs seq start fin] is the largest list of tuples [(a,b)] such
    that [valid_pairs a b] is true. *)
let rec max_pairs (seq : string) (start : int) (fin : int) =
  if start >= fin then []
  else
    [
      (max_pairs seq (start + 1) (fin - 1)
      @ if valid_pair seq start fin then [ (start, fin) ] else []);
    ]
    @ []
    |> largest_lst

(** [split seq start fin k]* is the list of tuples obtained by applying
    get_pairs to the strings obtained by chopping seq at position [k] *)
(* and split seq start fin k = max_pairs seq start k @ max_pairs seq (k
   + 1) fin *)

(** [find_max seq start fin k] is the largest list obtained by [split]
    after chopping [seq] at all of the possible k values*)
(* and find_max seq start fin k prev_max = let l = split seq start fin k
   in if k = start then if List.length prev_max > List.length l then
   prev_max else l else if List.length prev_max > List.length l then
   find_max seq start fin (k - 1) prev_max else find_max seq start fin
   (k - 1) l *)

(** [assoc_to_array seq pairs] is the [Array.t] in which for each entry
    [(i,j)] in [pairs], the entry at position [i] is [j], the entry at
    position [j] is [i] and all other entries are.

    Requires: No value is found in more than one entry of [pairs]. *)
let assoc_to_array seq (pairs : (int * int) list) =
  let arr = Array.make (String.length seq) 0 in
  List.iter
    (fun (a, b) ->
      Array.set arr a b;
      Array.set arr b a)
    pairs;
  arr

(** [nussinov r] is a secondary structure for [r] with maximum number of
    valid base pairs given by Nussinov's prediction algorithm. *)
let nussinov (r : Rna.t) =
  let pairs_assoc =
    max_pairs (Rna.get_seq r) 0 (String.length (Rna.get_seq r) - 1)
  in
  {
    seq = Rna.get_seq r;
    pairs = assoc_to_array (Rna.get_seq r) pairs_assoc;
    name = Rna.get_name r ^ " Secondary Structure";
    has_pseudoknot = false;
    info = Rna.get_info r;
  }

let predict r =
  try nussinov r |> rep_ok
  with Invalid_RI ->
    failwith "Invalidated RNA secondary struture rep invariant."

let get_seq r = r.seq
let get_info r = r.info
let get_name r = r.name

let to_dot_string r =
  r.pairs
  |> mapi (fun i j -> if i > j then "(" else if i < j then ")" else ".")
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
