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
    with index [i]. If no base pairing at [i], [get r.pairs i = -1]. The
    string [r.name] represents the RNA sequence name. [r.attributes]
    represents the RNA sequence information.

    Representation invariant: [r.seq] only consists of characters 'A',
    'G', 'C', or 'U'. Length of pairs is the lenght of [r.seq]. Pairs
    relation is symmetric, i.e. if [j = get r.pairs i] then
    [i = get r.pairs j]. Also [i <> get r.pairs i]. *)

exception Invalid_RI

(** [check p i j] is true if [i] is not [j], [i,j] in
    [\[0..length pairs-1\]], [p.i = j] and [p.j = i]. Otherwise,
    [check p i j] is false. *)
let check pairs i j =
  i <> j && 0 <= j
  && j < length pairs
  && 0 <= i
  && i < length pairs
  && i = get pairs j
  && j = get pairs i

(** [rep_ok r] is [r] if [r] satisfies the [rna_sec_str] representation
    invariant. Otherwise [rep_ok r] raises [Invalid_RI] *)
let rep_ok r =
  if
    length r.pairs = String.length r.seq
    && fold_left ( && ) true
         (mapi
            (fun i j -> if j = ~-1 then true else check r.pairs i j)
            r.pairs)
    && not (Str.string_match (Str.regexp "[^AGCU]+") r.seq 0)
  then r
  else raise Invalid_RI

(** [valid_pairs s i j] is true if and only if s.[i] and s.[j] form one
    of the 4 combinations which form a valid RNA base pair.

    Raises: [Invalid_argument] if [i] or [j] do not lie between [0] and
    [String.length seq - 1] inclusive. *)
let valid_pair (seq : string) i j =
  if i >= 0 && i < String.length seq && j >= 0 && j < String.length seq
  then
    match (seq.[i], seq.[j]) with
    | 'A', 'U' -> true
    | 'U', 'A' -> true
    | 'G', 'C' -> true
    | 'C', 'G' -> true
    | 'U', 'G' -> true
    | 'G', 'U' -> true
    | _ -> false
  else Invalid_argument "Invalid string position" |> raise

(** [largest_lst l] is one of the lists in [l], a list of lists, which
    has maximum length. *)
let rec largest_lst = function
  | h :: t ->
      let lt = largest_lst t in
      if List.length h > List.length lt then h else lt
  | [] -> []

(** [max_pairs seq start fin] is the largest list of tuples [(a,b)] such
    that [valid_pair seq a b] is true, . *)
let rec max_pairs (seq : string) (start : int) (fin : int) =
  if start >= fin then []
  else
    ([
       (max_pairs seq (start + 1) (fin - 1)
       @ if valid_pair seq start fin then [ (start, fin) ] else []);
     ]
    @
    try
      List.init
        (fin - start - 2)
        (fun k ->
          max_pairs seq start (start + k + 1)
          @ max_pairs seq (start + k + 2) fin)
    with Invalid_argument _ -> [])
    |> largest_lst

(** [assoc_to_array seq pairs] is the [Array.t] in which for each entry
    [(i,j)] in [pairs], the entry at position [i] is [j], the entry at
    position [j] is [i] and all other entries are.

    Requires: No value is found in more than one entry of [pairs]. *)
let assoc_to_array seq (pairs : (int * int) list) =
  let arr = Array.make (String.length seq) ~-1 in
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
  rep_ok
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
