(* [create_file f] creates empty file with name [f] if [f] does not
   exist. Otherwise, clears file [f]. *)
let create_file f : unit =
  Unix.openfile f [ O_RDWR; O_TRUNC; O_CREAT ] 0o666 |> Unix.close

let to_dot_string r =
  if r |> Secondary.get_pairs |> Secondary.is_pknot then
    Invalid_argument "Cannot convert pseudoknot to dot" |> raise
  else
    r |> Secondary.get_pairs
    |> Array.mapi (fun i j ->
           if j = ~-1 then "." else if i < j then "(" else ")")
    |> Array.fold_left ( ^ ) ""

let to_dot file r =
  if r |> Secondary.get_pairs |> Secondary.is_pknot then
    Invalid_argument "Cannot convert pseudoknot to dot" |> raise
  else if Sys.file_exists file then
    Printf.printf "%s is being ovewritten\n" file;
  create_file file;
  let oc = open_out file in
  Printf.fprintf oc ">%s\n%s\n%s" (Secondary.get_name r)
    (Secondary.get_seq r) (to_dot_string r);
  close_out oc

let to_ct file r =
  if Sys.file_exists file then
    Printf.printf "%s is being overwritten\n" file;
  create_file file;

  let oc = open_out file in

  (* [print_ct_line i j] prints line [i] to output channel [oc] in .ct
     format where [seq.[i+1]] is paired to [j+1]. The offset is due to
     .ct format using 1-indexing. *)
  let print_ct_line i j =
    Printf.fprintf oc "%i %c %i %i %i %i\n" (i + 1)
      (Secondary.get_seq r).[i]
      i (i + 2) (j + 1) (i + 1)
  in
  Printf.fprintf oc "%i %s\n"
    (r |> Secondary.get_seq |> String.length)
    (Secondary.get_name r);
  r |> Secondary.get_pairs |> Array.iteri print_ct_line;
  close_out oc
