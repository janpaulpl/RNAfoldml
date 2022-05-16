(** Input functions for secondary structures. *)

val from_dot_string : Rna.t -> string -> Secondary.t
(** [from_dot_string rna dots] is the secondary structure of RNA
    sequence [rna] with pairing determined by [dots]. [dots] is a string
    in valid dot-bracket notation.

    Raises: [Invalid_argument] exception if the
    [string.length rna.seq <> string.length dots] or if any pairs
    determined by [dots] are invalid. *)

val from_dot : string -> Secondary.t
(** [from_dot f] is the secondary structure contained in dot file [f].

    Raises: [Not_found] if [f] [f] does not exist and [Invalid_argument]
    exception if [f] is not in a valid dot format. *)

val from_ct : string -> Secondary.t
(** [from_ct f] is the secondary structure contained in ct file [f].

    Raises: [Not_found] if [f] [f] does not exist and [Invalid_argument]
    exception if [f] is not in a valid ct format. *)
