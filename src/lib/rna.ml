type t = {
  seq : string;
  name : string;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] contains only characters
    from ['A', 'G', 'C', 'U'] and [r.name] does not contain whitespace. *)

exception Invalid_RI

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

(** [rep_ok r] is [r] if [r] passes the rep invariant.

    Raises: [Invalid_RI] if [r] does not pass rep invariant. *)
let rep_ok r =
  if
    is_rna_seq r.seq
    && (not (String.contains r.name '\r'))
    && (not (String.contains r.name '\n'))
    && (not (String.contains r.name '\t'))
    && not (String.contains r.name ' ')
  then r
  else raise Invalid_RI

(** [remove_whitespace s] is the string [s] with any characters from
    [\['\r';'\n';'\t';' '\]] removed. *)
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
    let ch = open_in f in
    let s = really_input_string ch (in_channel_length ch) in
    close_in ch;
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