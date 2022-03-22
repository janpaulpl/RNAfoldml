(** Loading functions of secondary structure from ct and dot files or
    from strings in dot format. *)

val load_dot_string : Rna.t -> string -> Secondary.t
(** [load_dot_string rna dots] is the secondary structure of RNA
    sequence [rna] with pairing determined by [dots]. [dots] is a string
    in valid dot-bracket notation.

    Raises: [Invalid_argument] exception if the
    [string.length rna.seq <> string.length dots] or if any pairs
    determined by [dots] are invalid. *)

val load_dot : string -> Secondary.t
(** [load_dot f] is the secondary structure contained in dot file [f].

    Raises: [Invalid_argument] exception if [f] does not exist or if [f]
    is not in a valid dot format. *)

val load_ct : string -> Secondary.t
(** [load_ct f] is the secondary structure contained in ct file [f].

    Raises: [Invalid_argument] exception if [f] does not exist or if [f]
    is not in a valid ct format. *)
