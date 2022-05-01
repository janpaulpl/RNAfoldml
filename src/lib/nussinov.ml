(** [largest_lst l] is one of the lists in [l : 'a list list] which has
    maximum length. *)
let rec largest_lst = function
  | h :: t ->
      let lt = largest_lst t in
      if List.length h > List.length lt then h else lt
  | [] -> []

(** [max_pairs seq start fin memoize] is [memoize.(start).(fin)] if
    [memoize.(start).(fin) <> \[\]], else returns the largest list of
    tuples [(a,b)] such that for all [a,b], [start <= a < b <= fin] and
    [(seq.\[a\], seq.\[b\])] is a valid RNA pair.

    Effects: Sets [memoize.(start).(fin)] to
    [max_pairs seq start fin memoize] before returning. *)
let rec max_pairs
    (seq : string)
    (start : int)
    (fin : int)
    (memoize : (int * int) list array array) =
  if start >= fin then []
  else if memoize.(start).(fin) <> [] then memoize.(start).(fin)
  else
    let result =
      largest_lst
        ([
           (max_pairs seq (start + 1) (fin - 1) memoize
           @
           if Secondary.is_valid_pair seq.[start] seq.[fin] then
             [ (start, fin) ]
           else []);
         ]
        @
        try
          List.init
            (fin - start - 2)
            (fun k ->
              max_pairs seq start (start + k + 1) memoize
              @ max_pairs seq (start + k + 2) fin memoize)
        with Invalid_argument _ -> [])
    in
    memoize.(start).(fin) <- result;
    result

let predict (r : Rna.t) =
  Array.make_matrix (String.length r.seq) (String.length r.seq) []
  |> max_pairs r.seq 0 (String.length r.seq - 1)
  |> Secondary.make r
