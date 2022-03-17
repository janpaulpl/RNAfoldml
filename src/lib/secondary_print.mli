(** Output functions for secondary structures. *)

val to_dot_string : Secondary.t -> string
(** [to_dot_string r] is the secondary structure [r] in dot-bracket
    notation. A dot, ['.'], indicates an unpaired nucleotide, ['(']
    indicates a 5' nucleotide and [')'] represents a 3' nucleotide.

    Requires: [r] contains no pseudo-knots

    Example: ["((...))()"] is a possible secondary structure for the
    sequence AUGGGAUCG. *)

val to_dot : string -> Secondary.t -> unit
(** [to_dot f r] saves [r] to file [f] in .dot format.

    Raises: [Invalid_argument] exception if [f] is not a valid .ct file
    name or does not exist. *)

val to_ct : string -> Secondary.t -> unit
(** [write_ct f r] saves the rna secondary structure [r] in connectivity
    table (.ct) format in file [f].

    Raises: [Invalid_argument] exception if [f] is not a valid .ct file
    name or does not exist. *)