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

let rna_from_fasta s =
  let x : t list = [ rep_ok { seq = ""; name = "sdas"; info = "" } ] in
  if s = "" then x else x

let rna_from_string s name =
  try rep_ok { seq = s; name; info = "" }
  with Invalid_RI ->
    Invalid_argument "Unable to parse RNA sequence" |> raise

let get_seq r = r.seq
let get_info r = r.info
let get_name r = r.name

(* (** Temporary read from FASTA file function. Returns the FASTA as a
   string.*) let read_lines file process = let in_ch = open_in file in
   let rec read_line () = let line = try input_line in_ch with
   End_of_file -> exit 0 in process line; read_line () in read_line
   () *)
