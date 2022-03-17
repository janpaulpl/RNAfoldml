open Array

type t = {
  seq : string;
  pairs : int array;
  name : string;
  has_pseudoknot : bool option;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. [r.pairs i] is the index of the predicted base pairing
    with index [i]. If no base pairing at [i], [get r.pairs i = -1]. The
    string [r.name] represents the RNA sequence name.

    Representation invariant: [r.seq] only consists of characters 'A',
    'G', 'C', or 'U'. Length of pairs is the length of [r.seq]. Pairs
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

(** [rep_ok r] is [r] if [r] satisfies the [t] representation invariant.
    Otherwise [rep_ok r] raises [Invalid_RI] *)
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

(** [valid_indices s i j] is true if and only if [i] and [j] are in the
    range [0..String.length s - 1]. *)
let valid_indices s i j =
  i >= 0 && i < String.length s && j >= 0 && j < String.length s

(** [is_valid s i j] is true if and only if s.[i] and s.[j] form one of
    the 4 combinations which form a valid RNA base pair.

    Raises: [Invalid_argument] if [i] or [j] do not lie between [0] and
    [String.length seq - 1] inclusive. *)
let is_valid (seq : string) i j =
  if valid_indices seq i j |> not then
    Invalid_argument "Invalid string position" |> raise
  else
    match (seq.[i], seq.[j]) with
    | 'A', 'U' -> true
    | 'U', 'A' -> true
    | 'G', 'C' -> true
    | 'C', 'G' -> true
    | 'U', 'G' -> true
    | 'G', 'U' -> true
    | _ -> false

(** [largest_lst l] is one of the lists in [l], a list of lists, which
    has maximum length. *)
let rec largest_lst = function
  | h :: t ->
      let lt = largest_lst t in
      if List.length h > List.length lt then h else lt
  | [] -> []

(** [max_pairs seq start fin] is the largest list of tuples [(a,b)] such
    that [is_valid seq a b] is true. *)
let rec max_pairs
    (seq : string)
    (start : int)
    (fin : int)
    (computed : (int * int) list array array) =
  if start >= fin then []
  else
    let precomputed = Array.get (Array.get computed start) fin in
    if precomputed <> [] then precomputed
    else
      let result =
        ([
           (max_pairs seq (start + 1) (fin - 1) computed
           @ if is_valid seq start fin then [ (start, fin) ] else []);
         ]
        @
        try
          List.init
            (fin - start - 2)
            (fun k ->
              max_pairs seq start (start + k + 1) computed
              @ max_pairs seq (start + k + 2) fin computed)
        with Invalid_argument _ -> [])
        |> largest_lst
      in
      computed.(start).(fin) <- result;
      result

(** [assoc_to_array seq pairs] is the [Array.t] in which for each entry
    [(i,j)] in [pairs], the entry at position [i] is [j], the entry at
    position [j] is [i] and all other entries are.

    Requires: No value is found in more than one entry of [pairs]. Every
    vallue is a valid index of seq. *)
let assoc_to_array seq (pairs : (int * int) list) =
  let arr = Array.make (String.length seq) ~-1 in
  List.iter
    (fun (a, b) ->
      assert (valid_indices seq a b);
      Array.set arr a b;
      Array.set arr b a)
    pairs;
  arr

(** [nussinov r] is a secondary structure for [r] which maximumizes the
    number of valid base pairs given by Nussinov's prediction algorithm. *)
let nussinov (r : Rna.t) =
  let pair_matrix =
    make_matrix (String.length r.seq) (String.length r.seq) []
  in

  let pairs_assoc =
    max_pairs r.seq 0 (String.length r.seq - 1) pair_matrix
  in
  rep_ok
    {
      seq = r.seq;
      pairs = assoc_to_array r.seq pairs_assoc;
      name = r.name ^ "-nussinov-sec-struct";
      has_pseudoknot = Some false;
    }

let predict r =
  try nussinov r |> rep_ok
  with Invalid_RI ->
    failwith "Invalidated RNA secondary struture rep invariant."

let get_seq r = r.seq
let get_name r = r.name
let get_pairs r = r.pairs |> copy