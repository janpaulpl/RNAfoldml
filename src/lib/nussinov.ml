(** [largest_lst l] is one of the lists in [l : 'a list list] which has
    maximum length. *)
let largest_lst lst =
  List.fold_left
    (fun x y -> if List.length y > List.length x then y else x)
    [] lst

let max_pairs seq : (int * int) list =
  let size = String.length seq in
  let memo : (int * int) list array array =
    Array.make_matrix size size []
  in
  (* [max_pairs_rec (start, fin)] is [memo.(start).(fin)] if
     [memo.(start).(fin) <> ~-1], else returns the maximum number of
     tuples [(a,b)] such that for all [a,b], [start <= a < b <= fin] and
     [(seq.\[a\], seq.\[b\])] is a valid RNA pair.

     Effects: Fills in [memo.(x).(y)] for all recursive calls
     [max_pairs_rec (x, y)]. *)
  let rec max_pairs_rec : int * int -> (int * int) list = function
    | start, fin when start >= fin -> []
    | start, fin when memo.(start).(fin) <> [] -> memo.(start).(fin)
    | start, fin ->
        memo.(start).(fin) <-
          ([
             (if Secondary.is_valid_pair seq.[start] seq.[fin] then
              (start, fin) :: max_pairs_rec (start + 1, fin - 1)
             else max_pairs_rec (start + 1, fin - 1));
           ]
          @
          try
            List.init (fin - start) (fun k ->
                max_pairs_rec (start, start + k)
                @ max_pairs_rec (start + k + 1, fin))
          with Invalid_argument _ -> [])
          |> largest_lst;
        memo.(start).(fin)
  in
  max_pairs_rec (0, size - 1)

let predict (r : Rna.t) = max_pairs r.seq |> Secondary.make r
