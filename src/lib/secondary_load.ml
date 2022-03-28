let read_file f =
  let c = open_in f in
  let s =  in_channel_length c |> really_input_string c in
  close_in c; s

(** Temporary pairs extract helper function, 
    have to make this anon later. *)
let head (a,b,c) = a

let load_dot_string rna dots =
  if String.length (Secondary.get_seq rna) <> String.length dots
  then Invalid_argument "Unable to parse RNA sequence" |> raise
  else Secondary.assoc_to_array (String.length dots) 
    (head (List.fold_left
      (fun (pairs, left_parenths, index as acc) c  ->
        match c with
        | '(' -> pairs, index :: left_parenths, index + 1
        | ')' -> (match left_parenths with 
          | h::t -> ((h, index) :: pairs, t, index + 1) 
          | _ -> Invalid_argument "Invalid dot string" |> raise)
        | '.' -> acc
        | _ -> Invalid_argument "Invalid character in dot string" |> raise)
      ([], [], 0) (List.init (String.length dots) (String.get dots))))

let load_dot f : Secondary.t =
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"

let load_ct f =
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"
