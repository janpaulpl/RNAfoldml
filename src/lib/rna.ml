type t = {
  seq : string;
  name : string;
  info : string;
}
(** Abstraction function: The string [r.seq] represents a valid RNA
    sequence. Representation invariant: [r.seq] only contains characters *)

exception Invalid_RI

(** [rep_ok r] is [r] if [r] passes the rep invariant.

    Raises: [Failure] if [r] does not pass rep invariant. *)
let rep_ok r =
  if Str.string_match (Str.regexp "[^AGCU]+") r.seq 0 then
    raise Invalid_RI
  else r

(** [string_from_fasta] is the parsed data from the fasta file as a string. Gets rid of 
    whitespace and [;] comments. *)
let string_from_fasta filename = let ch = open_in filename in 
  let string_fasta = really_input_string ch (in_channel_length ch) in 
  close_in ch; 
  (fun r -> Str.global_replace (Str.regexp ";[^\n]*\n") "" string_fasta) "" 
  |> Str.global_replace (Str.regexp " ") ""

(** [get_from_fasta] searches for a particular regex in a given sequence string*)
let get_from_fasta s rgx = 
  Str.global_replace (Str.regexp rgx) "\\1" s

(** [rna_from_single_fasta] builds a [t] from a single sequence (string). *)
let rna_from_single_fasta s = 
  { 
    seq =  get_from_fasta s "\\([AGCU]*\\)[^\n]*\n*"; 
    name = get_from_fasta s ">[^-]*-" ; (* This returns literally the opposite *)
    info =  get_from_fasta s "\\(-[^\n]*\n\\)"; (* This too *)
  }
  
(** [rna_from_fasta] is the [t list] with [i] sequence: 
    name, information, and sequence data). *)
let rna_from_fasta f =
  let s = string_from_fasta f in
  let sequences = String.split_on_char '>' s in
  let x : t list = List.map rna_from_single_fasta sequences in
    x
    |> List.map (fun r -> try [rep_ok r] with Invalid_RI -> [])  
    |> List.flatten

let rna_from_string s name =
  try rep_ok { seq = s; name; info = "" }
  with Invalid_RI ->
    Invalid_argument "Unable to parse RNA sequence" |> raise

let get_seq r = r.seq
let get_info r = r.info
let get_name r = r.name

