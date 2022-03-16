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

(** Remove trailing [\n] character *)
let remove_trail = Str.global_replace (Str.regexp_string "\n") ""
(** Remove whitespace in string *)
let remove_whitespace = Str.global_replace (Str.regexp "[\r\n\t ]") ""

(** [get_data s g] captures the necessary fields from a string [s] to build [t]. 
    [g] is a field in [t] represented by a regex capture group. 
    [Capture Group 1] : [\\(.*\n\\)] -> [name], 
    [Capture Group 2] : [\\([AGCU ]+\n?[AGCU\n ]+\\)] -> [seq]. 
    
    [name] CANNOT start with whitespace.*)
let get_rna_fields s g =
  let rgx = Str.regexp "\\(.*\n\\)\\([AGCU ]+\n?[AGCU\n ]+\\)" in
  match Str.string_match rgx s 0 with
  | true -> Str.matched_group g s
  | false -> Invalid_argument "Invalid FASTA sequence" |> raise

(** [rna_from_single_fasta s] is the rna of type [t] represented by the
    single entry fasta style string [s]. *)
let rna_from_single_fasta s =
  { 
  name = get_rna_fields s 1 |> remove_trail ; 
  info = ""; 
  seq = get_rna_fields s 2 |> remove_trail |> remove_whitespace }


(** [from_fasta] is the [t list] with [i] sequence: name,
    information, and sequence data). *)
let from_fasta f =
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