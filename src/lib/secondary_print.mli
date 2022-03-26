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

    Raises: [Invalid_argument] exception if [f] cannot be created. *)

val to_ct : string -> Secondary.t -> unit
(** [to_ct f r] saves [r] to file [f] in .ct format.

    Raises: [Invalid_argument] exception if [f] cannot be created. *)