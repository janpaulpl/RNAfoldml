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
  else dots |> dot_to_assoc |> Secondary.make rna

let from_dot f : Secondary.t =
  if not (Sys.file_exists f) then raise Not_found
  else
    try
      let ic = open_in f in
      let name = input_line ic in
      let seq = input_line ic in
      let dot =
        really_input_string ic
          (in_channel_length ic - String.length name - String.length seq
         - 2)
      in
      close_in ic;
      dot |> from_dot_string (Rna.from_string seq name)
    with
    | Invalid_argument m -> Invalid_argument m |> raise
    | _ ->
        Invalid_argument
          ("Unable to load secondary structure from dot file: " ^ f)
        |> raise

(** [from_ct_string name ct_lines] is the rna secondary structure with
    name [name] and sequence and base pairs determined by the
    connectivity table whose lines are elements of [ct_lines].

    Raises: [Invalid_argument] if [ct_lines] is not a valid formatted ct
    table. *)
let from_ct_string (name : string) (ct_lines : string list) =
  (* Stores first 3 entries of each line in triple list. *)
  let triples =
    ct_lines
    |> List.filter (fun l -> l <> "")
    |> List.map (fun line ->
           ignore
             (Str.search_forward
                (Str.regexp
                   "[0-9]+ \\([AGCU]\\) \\([0-9]+\\) [0-9]+ \
                    \\([0-9]+\\) [0-9]+")
                line 0);
           ( Str.matched_group 1 line,
             Str.matched_group 2 line,
             Str.matched_group 3 line ))
  in
  triples
  |> List.map (fun (_, b, c) -> (int_of_string b, int_of_string c - 1))
  |> List.filter (fun (i, j) -> i <= j)
  |> Secondary.make
       (Rna.from_string
          (List.map (fun (a, _, _) -> a) triples |> String.concat "")
          name)

let from_ct f : Secondary.t =
  if not (Sys.file_exists f) then raise Not_found
  else
    try
      let ic = open_in f in
      let file = really_input_string ic (in_channel_length ic) in
      close_in ic;
      match String.split_on_char '\n' file with
      | [] -> Invalid_argument "Empty File" |> raise
      | name_line :: ct_lines -> (
          match String.split_on_char ' ' name_line with
          | [ _; name ] -> from_ct_string name ct_lines
          | _ ->
              Invalid_argument "First line in invalid format" |> raise)
    with
    | Invalid_argument m -> Invalid_argument m |> raise
    | _ ->
        Invalid_argument
          "Unable to load secondary structure in from ct file" |> raise
