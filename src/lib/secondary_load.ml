(* let read_file f =
  let c = open_in f in
  let s =  in_channel_length c |> really_input_string c in
  close_in c; s *)

(** Temporary pairs extract helper function, 
    have to make this anon later. *)
let fst3 (a,_,_) = a

let load_dot_string rna dots =
  if String.length (Rna.get_seq rna) <> String.length dots
  then Invalid_argument "Unable to parse RNA sequence" |> raise
  else  Secondary.make_t ("") 
  (Secondary.assoc_to_array (String.length dots) 
    (fst3 (List.fold_left
      (fun (pairs, left_parenths, index as acc) c  ->
        match c with
        | '(' -> pairs, index :: left_parenths, index + 1
        | ')' -> (match left_parenths with 
          | h::t -> ((h, index) :: pairs, t, index + 1) 
          | _ -> Invalid_argument "Invalid dot string" |> raise)
        | '.' -> acc
        | _ -> Invalid_argument "Invalid character in dot string" |> raise)
      ([], [], 0) (List.init (String.length dots) (String.get dots))))) 
      ("") (None)

let load_dot f : Secondary.t =
  (* let s = read_file f |> String.split_on_char '\n' in  *)
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"

let load_ct f =
  (* let s = read_file f |> String.split_on_char '\n' in  *)
  if f = "" then raise (Failure "not implemented")
  else failwith "nope still not"
