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

(** [is_valid_pair i j] is true if and only if characters [(i,j)] form
    one of 2 Watson-Crick RNA pairs: 'A','U' or 'C','G'. Order does not
    effect result. *)
let is_valid_pair i j =
  match (i, j) with
  | 'A', 'U' -> true
  | 'U', 'A' -> true
  | 'G', 'C' -> true
  | 'C', 'G' -> true
  | _ -> false

(** [rep_ok r] is [r] if [r] satisfies the [t] representation invariant,
    otherwise raises [Invalid_RI]. Runs in O(n) where
    [n = String.length r.seq]. *)
let rep_ok r =
  let has_valid_pairs r =
    Array.fold_left ( && ) true
      (Array.mapi
         (fun i j ->
           if j + 1 = 0 then true
           else
             i <> j && 0 <= j
             && j < String.length r.seq
             && 0 <= i
             && i < String.length r.seq
             && i = r.pairs.(j)
             && j = r.pairs.(i)
             && is_valid_pair r.seq.[i] r.seq.[j])
         r.pairs)
  in
  if
    String.length r.seq == Array.length r.pairs
    && has_valid_pairs r && Rna.is_rna_seq r.seq
  then r
  else raise Invalid_RI

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
  let cartesian l =
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l) l)
  in
  List.init (Array.length pairs - 1) (fun x -> x)
  |> cartesian
  |> List.exists (fun (x, y) ->
         if y <= x then false
         else condition1 pairs x y && condition2 pairs x y)

let is_simple_pknot_arr pairs =
  let cartesian l =
    List.concat (List.map (fun e -> List.map (fun e' -> (e, e')) l) l)
  in
  List.init (Array.length pairs - 1) (fun x -> x)
  |> cartesian
  |> List.filter (fun (x, y) ->
         if y <= x then false
         else condition1 pairs x y && condition2 pairs x y)

let distance r1 r2 =
  let r1, r2 = (rep_ok r1, rep_ok r2) in
  let all_0 = Array.for_all (fun i -> i = 0) in
  if all_0 r1.pairs || all_0 r2.pairs then Int.max_int
  else
    let min_dist_r2 i1 j1 =
      Array.mapi (fun i2 j2 -> max (abs i1 - i2) (abs j1 - j2)) r2.pairs
      |> Array.fold_left min Int.max_int
    in
    r1.pairs |> Array.mapi min_dist_r2 |> Array.fold_left max 0

let similarity r1 r2 =
  if String.length r1.seq != String.length r2.seq then
    Invalid_argument
      "Cannot calculate similarity of different length rna structures"
    |> raise
  else
    r1.pairs
    |> Array.map2 (fun i j -> if i = j then 1. else 0.) r2.pairs
    |> Array.fold_left ( +. ) 0.
    |> ( /. ) (r1.pairs |> Array.length |> float_of_int)

let get_seq r = r.seq
let get_name r = r.name
let get_pairs r = r.pairs |> Array.copy
let get_rna r = Rna.from_string r.seq r.name

let make (rna : Rna.t) (pairs : (int * int) list) =
  try
    rep_ok
      {
        name = rna.name;
        seq = rna.seq;
        pairs = assoc_to_array (String.length rna.seq) pairs;
      }
  with Invalid_RI ->
    Invalid_argument "Pairing is not valid for Rna.t" |> raise
