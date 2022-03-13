(** Representation of rna secondary structure predictions.

    This module represents the predicted secondary structure of rna
    sequences and associated information such as the sequence name, the
    nucleotide sequence, and any associated information. It also handles
    functions for getting secondary structure predictions from rna
    values. *)

type rna_sec_str
(** The abstract type of values representing an RNA sequence and its
    secondary structure. *)

val predict : Rna.t -> rna_sec_str
(** [get_sec_str r] is the secondary structure predictions for the rna
    [r] *)

val get_seq : rna_sec_str -> string
(** [get_seq r] is the string of bases stored in the sequence of [r]. *)

val get_info : rna_sec_str -> string
(** [get_info r] is a string containing information about [r]. *)

val get_name : rna_sec_str -> string
(** [get_name r] is the name of [r]. *)

val to_dot_string : rna_sec_str -> string
(** [to_dot_string r] is the secondary structure [r] in dot-bracket
    notation. A dot, ['.'], indicates an unpaired nucleotide, ['(']
    indicates a 5' nucleotide and [')'] represents a 3' nucleotide.

    Requires: [r] contains no pseudo-knots

    Example: ["((...))()"] is a possible secondary structure for the
    sequence AUGGGAUCG. *)

val to_ct : string -> rna_sec_str -> unit
(** [write_ct f r] saves the rna secondary structure [r] in connectivity
    table (.ct) format in file [f]. If [f] already exists, then replaces
    the contents of [f].

    Raises: [Invalid_argument] exception if [f] is not a valid file .ct
    file name.

    Effects: Prints warning message to terminal if [f] already exists,
    then replaces the contents of [f]. *)
