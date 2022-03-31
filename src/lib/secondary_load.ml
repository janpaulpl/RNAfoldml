
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
    dots |> dot_to_assoc |> Secondary.make rna 

let from_dot f : Secondary.t =
  if not (Sys.file_exists f) then
    Invalid_argument ("Cannot find file: " ^ f) |> raise
  else
    try
      let ic = open_in f in
      let name = input_line ic in
      let seq = input_line ic in
      let dot = input_line ic in
      let () = close_in ic in
        dot |> from_dot_string (Rna.from_string seq name)
    with Invalid_argument m -> Invalid_argument m |> raise

let ct_rgxp = Str.regexp "[0-9]+ ([AGCU]) ([0-9]+) [0-9]+ ([0-9]+) [0-9]+"

let kill _ = ()

let from_ct f : Secondary.t =
  if not (Sys.file_exists f) then
    Invalid_argument ("Cannot find file: " ^ f) |> raise
  else 
    let ic = open_in f in
    let file = really_input_string ic (in_channel_length ic) in
    match String.split_on_char '\n' file with 
    | [] ->  Invalid_argument "Empty File" |> raise
    | name::t -> 
    let name = name in 
    let triples = List.map (fun s -> kill (Str.search_forward ct_rgxp s 0); 
    (Str.matched_group 1 s, Str.matched_group 2 s, Str.matched_group 3 s)) t in 
    let pairs = List.map 
    (fun (_,b,c) -> int_of_string b, int_of_string c) triples in
    let seq = List.map 
    (fun (a,_,_) -> a) triples |> String.concat "" in
    let () = close_in ic in 
    Secondary.make (Rna.from_string seq name) pairs