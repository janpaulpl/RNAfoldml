type t = {
  name : string;
  seq : string;
  pairs : int array;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. [r.pairs i] is the index of the predicted base pairing
    with index [i]. If no base pairing at [i], [get r.pairs i = -1].
    [r.name] is the RNA sequence name. Representation invariant: [r.seq]
    only consists of characters 'A', 'G', 'C', or 'U'. Length of pairs
    is the length of [r.seq]. Pairs relation is symmetric, i.e. if
    [j = get r.pairs i] then [i = get r.pairs j]. Also
    [i <> get r.pairs i]. *)

exception Invalid_RI

(** [assoc_to_array size pairs] is the array [a]] with [a.i = j] if
    [(i,j)] or [(j,i)] in [pairs] and remaining entries are [-1].
    Requires: For all [(i,j)] in [pairs], [i] and [j] are in
    [0..size-1]. No value appears in more than one [pair]. *)
let assoc_to_array size (pairs : (int * int) list) =
  let arr = Array.make size ~-1 in
  List.iter
    (fun (a, b) ->
      assert (0 <= a && 0 <= b && a < size && b < size);
      assert (~-1 = arr.(a) && ~-1 = arr.(b));
      Array.set arr a b;
      Array.set arr b a)
    pairs;
  arr

(** [is_valid_base_pair s i j] is true if and only if [s.\[i\]] and
    [s.\[j\]] form one of the 4 combinations which form a valid RNA base
    pair. Raises: [Invalid_argument] if [i] or [j] are not in
    [0,...,String.length seq - 1]. *)
let is_valid_base_pair seq i j =
  try
    match (seq.[i], seq.[j]) with
    | 'A', 'U' -> true
    | 'U', 'A' -> true
    | 'G', 'C' -> true
    | 'C', 'G' -> true
    | _ -> false
  with _ -> Invalid_argument "Invalid string position" |> raise

(** [rep_ok r] is [r] if [r] satisfies the [t] representation invariant.
    Otherwise [rep_ok r] raises [Invalid_RI] *)
let rep_ok r =
  let has_valid_pairs r =
    Array.fold_left ( && ) true
      (Array.mapi
         (fun i j ->
           if j + 1 = 0 then true
           else
             String.length r.seq == Array.length r.pairs
             && i <> j && 0 <= j
             && j < String.length r.seq
             && 0 <= i
             && i < String.length r.seq
             && i = r.pairs.(j)
             && j = r.pairs.(i)
             && is_valid_base_pair r.seq i j)
         r.pairs)
  in

  let is_rna_seq s =
    String.map
      (fun c ->
        match c with
        | 'U' -> ' '
        | 'A' -> ' '
        | 'G' -> ' '
        | 'C' -> ' '
        | _ -> 'a')
      s
    = String.make (String.length s) ' '
  in
  if has_valid_pairs r && is_rna_seq r.seq then r else raise Invalid_RI

(** [largest_lst l] is one of the lists in [l], a list of lists, which
    has maximum length. *)
let rec largest_lst = function
  | h :: t ->
      let lt = largest_lst t in
      if List.length h > List.length lt then h else lt
  | [] -> []

(* ------------ Functions Suite for Nussinov Algorithm ------------ *)

(** [max_pairs seq start fin] is the largest list of tuples [(a,b)] such
    that [is_valid seq a b] is true. *)
let rec max_pairs
    (seq : string)
    (start : int)
    (fin : int)
    (computed : (int * int) list array array) =
  if start >= fin then []
  else
    let precomputed = computed.(start).(fin) in
    if precomputed <> [] then precomputed
    else
      let result =
        ([
           (max_pairs seq (start + 1) (fin - 1) computed
           @
           if is_valid_base_pair seq start fin then [ (start, fin) ]
           else []);
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

(** [nussinov r] is a secondary structure for [r] which maximumizes the
    number of valid base pairs given by Nussinov's prediction algorithm. *)
let nussinov (r : Rna.t) =
  let optimal_pair_matrix =
    Array.make_matrix (String.length r.seq) (String.length r.seq) []
  in

  let pairs_assoc =
    max_pairs r.seq 0 (String.length r.seq - 1) optimal_pair_matrix
  in
  rep_ok
    {
      seq = r.seq;
      pairs = assoc_to_array (String.length r.seq) pairs_assoc;
      name = r.name ^ "-nussinov-sec-struct";
    }

let is_pknot r =
  let rec is_pknot_helper pairs index stack =
    if index = Array.length pairs then (
      assert (stack = []);
      false)
    else
      match pairs.(index) with
      | -1 -> is_pknot_helper pairs (index + 1) stack
      | twin when twin > index ->
          is_pknot_helper pairs (index + 1) (twin :: stack)
      | twin when twin < index && List.hd stack <> index -> true
      | twin when twin < index ->
          is_pknot_helper pairs (index + 1) (List.tl stack)
      | _ -> raise Invalid_RI
  in
  is_pknot_helper r 0 []

let rec check_index
    (pairs : int array)
    (cut1 : int)
    (cut2 : int)
    (index : int) =
  let twin = pairs.(index) in
  if index = 0 && (twin = -1 || (twin > cut1 && twin <= cut2)) then true
  else if
    twin = -1
    || (index < cut1 && cut1 < twin && twin <= cut2)
    || (index = cut1 && twin > cut2)
    || (index > cut1 && index < cut2 && (twin < cut1 || twin > cut2))
    || (index = cut2 && twin < cut1)
    || (index > cut2 && twin >= cut1 && twin < cut2)
  then check_index pairs cut1 cut2 (index - 1)
  else false

let condition1 (pairs : int array) (cut1 : int) (cut2 : int) =
  check_index pairs cut1 cut2 (Array.length pairs - 1)

let condition2 (pairs : int array) (cut1 : int) (cut2 : int) =
  let stack_pair = Stack.create () in
  let rec process_pairs pairs cut fin index stack (left : bool) =
    let twin = pairs.(index) in
    if index >= fin then
      Stack.length stack = 0 || Stack.pop stack = index
    else if twin = -1 then
      process_pairs pairs cut fin (index + 1) stack left
    else if index < cut then
      if (not left) && twin < cut then
        process_pairs pairs cut fin (index + 1) stack left
      else
        let () = Stack.push twin stack in
        process_pairs pairs cut fin (index + 1) stack left
    else if index = cut then false
    else if index > cut then
      if left && twin > cut then
        process_pairs pairs cut fin (index + 1) stack left
      else if Stack.is_empty stack then false
      else if index <> Stack.pop stack then false
      else process_pairs pairs cut fin (index + 1) stack left
    else false
  in
  process_pairs pairs cut1 cut2 0 stack_pair true
  && process_pairs pairs cut2
       (Array.length pairs - 1)
       cut1 stack_pair false

let is_simple_pknot pairs =
  let cartesian l l' =
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l') l)
  in
  let y = List.init (Array.length pairs - 1) (fun x -> x) in
  let lst = cartesian y y in
  List.exists
    (fun (x, y) ->
      if y <= x then false
      else condition1 pairs x y && condition2 pairs x y)
    lst

let predict r =
  try nussinov r |> rep_ok
  with Invalid_RI ->
    failwith "Invalidated RNA secondary struture rep invariant."

let distance r1 r2 =
  let r1, r2 = (rep_ok r1, rep_ok r2) in
  if Array.length r1.pairs = 0 || Array.length r2.pairs = 0 then
    Int.max_int
  else
    let min_dist_r2 i1 j1 =
      Array.mapi (fun i2 j2 -> max (abs i1 - i2) (abs j1 - j2)) r2.pairs
      |> Array.fold_left min Int.max_int
    in
    r1.pairs |> Array.mapi min_dist_r2 |> Array.fold_left max 0

let get_seq r = r.seq
let get_name r = r.name
let get_pairs r = r.pairs |> Array.copy

(** [dot_to_assoc d] is the association list of pairs represented by
    [d], a [string] in dot format. Raises: [Invalid_argument] if [d] is
    not in valid dot string format. Example: [dot_to_assoc "(())..()"]
    is [\[(0,3);(1,2);(6,7)\]]*)
let dot_to_assoc dot =
  let pair, _, _ =
    (* Algorithm iterates through the char list from the left,
       accumulating a triple: (<list of completed pairs>, <positions of
       unpaired left parentheses>, <current index>). *)
    List.fold_left
      (fun (pairs, left_parenths, index) c ->
        match c with
        | '(' -> (pairs, index :: left_parenths, index + 1)
        | ')' -> (
            match left_parenths with
            | h :: t -> ((h, index) :: pairs, t, index + 1)
            | _ -> Invalid_argument "Invalid dot string" |> raise)
        | '.' -> (pairs, left_parenths, index + 1)
        | _ -> Invalid_argument "Invalid char in dot string" |> raise)
      ([], [], 0)
      (List.init (String.length dot) (String.get dot))
  in
  pair

let from_dot_string (rna : Rna.t) (dots : string) =
  if String.length rna.seq <> String.length dots then
    Invalid_argument "Unable to parse RNA sequence" |> raise
  else
    rep_ok
      {
        seq = rna.seq;
        pairs =
          dots |> dot_to_assoc |> assoc_to_array (String.length dots);
        name = rna.name;
      }

let from_dot f : t =
  (* let s = read_file f |> String.split_on_char '\n' in *)
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"

let from_ct f : t =
  (* let s = read_file f |> String.split_on_char '\n' in *)
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"