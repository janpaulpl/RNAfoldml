type t = {
  seq : string;
  name : string;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

exception Invalid_RI

let only_bases s =
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

(** [rep_ok r] is [r] if [r] passes the rep invariant.

    Raises: [Invalid_RI] if [r] does not pass rep invariant. *)
let rep_ok r =
  if
    only_bases r.seq
    && (not (String.contains r.name '\r'))
    && (not (String.contains r.name '\n'))
    && (not (String.contains r.name '\t'))
    && not (String.contains r.name ' ')
  then r
  else raise Invalid_RI

(** [string_from_fasta] is the parsed data from the fasta file as a
    string. *)
let string_from_fasta filename =
  let ch = open_in filename in
  let string_fasta = really_input_string ch (in_channel_length ch) in
  let () = close_in ch in
  string_fasta

(** Remove whitespace in string *)
let remove_whitespace s =
  s
  |> String.split_on_char '\r'
  |> String.concat ""
  |> String.split_on_char '\n'
  |> String.concat ""
  |> String.split_on_char '\t'
  |> String.concat ""
  |> String.split_on_char ' '
  |> String.concat ""

(** [from_single_fasta s] is the rna of type [t] represented by the
    single entry fasta style string [s]. *)
let from_single_fasta s =
  try
    rep_ok
      {
        name = String.sub s 0 (String.index s '\n');
        seq =
          String.sub s (String.index s '\n')
            (String.length s - String.index s '\n')
          |> String.trim |> remove_whitespace;
      }
  with _ -> Invalid_argument "Invalid Single Fasta" |> raise

let from_fasta f =
  if not (Sys.file_exists f) then
    Invalid_argument ("Cannot find file: " ^ f) |> raise
  else
    let s = string_from_fasta f in
    let sequences =
      match String.split_on_char '>' s with
      | [] -> []
      | "" :: t -> t
      | h :: t -> if h.[0] = ';' then t else h :: t
    in
    let x : t list list =
      List.map
        (fun r ->
          try [ from_single_fasta r ] with Invalid_argument _ -> [])
        sequences
    in
    x |> List.concat

let from_string seq name =
  try rep_ok { seq; name }
  with Invalid_RI ->
    Invalid_argument "Unable to parse RNA sequence" |> raise