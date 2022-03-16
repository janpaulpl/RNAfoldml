type t = {
  seq : string;
  name : string;
  info : string;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

exception Invalid_RI

(** [rep_ok r] is [r] if [r] passes the rep invariant.

    Raises: [Invalid_RI] if [r] does not pass rep invariant. *)
let rep_ok r =
  if Str.string_match (Str.regexp "[^AGCU]+") r.seq 0 then
    raise Invalid_RI
  else r

(** [string_from_fasta] is the parsed data from the fasta file as a
    string. *)
let string_from_fasta filename =
  let ch = open_in filename in
  let string_fasta = really_input_string ch (in_channel_length ch) in
  close_in ch;
  string_fasta

(** [rna_from_single_fasta s] is the rna of type [t] represented by the
    single entry fasta style string [s].

    ex) name\nAAAUUUAUAUUA ex) >name info1 info2 info3\nAAUAUAUAU
    AUAUAUAUU\n AUAUAUAUAU AUAUAUAAUAUA\n*)
let rna_from_single_fasta s =
  let name_len =
    match String.index_opt s ' ' with
    | None -> Invalid_argument "Fasta file without name" |> raise
    | Some i -> i
  in
  { seq = s; name = String.sub s 0 name_len; info = "" }

(** [rna_from_fasta] is the [t list] with [i] sequence: name,
    information, and sequence data). *)
let rna_from_fasta f =
  let s = string_from_fasta f in
  let sequences =
    match String.split_on_char '>' s with
    | [] -> []
    | h :: t -> if String.length h > 0 && h.[0] = ';' then t else h :: t
  in
  let x : t list = List.map rna_from_single_fasta sequences in
  x
  |> List.map (fun r -> try [ rep_ok r ] with Invalid_RI -> [])
  |> List.flatten

let rna_from_string s name =
  try rep_ok { seq = s; name; info = "" }
  with Invalid_RI ->
    Invalid_argument "Unable to parse RNA sequence" |> raise