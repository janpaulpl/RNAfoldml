(** Visualization of RNA secondary strucutures. This library's functions
    only work on Unix systems. *)

val circle_graph : string -> Secondary.t -> unit
(** [circle_graph f r] saves a circular visualization of secondary
    structure [f] to file [f] in the current directory. Visualization
    includes [r1] name and number of base pairs.

    Raises: [Invalid_argument] if unable to save to filename [f]. *)

val compare_graph : string -> Secondary.t -> Secondary.t -> unit
(** [compare_graph f r1 r2] saves a circular visual comparison of
    secondary structures [f] to file [f] in the current directory.
    Includes the names of [r1,r2], percent similarity between the two
    sequences, and a key describing the coloring of pairs.

    Raises: [Invalid_argument] if unable to save to filename [f]. *)
