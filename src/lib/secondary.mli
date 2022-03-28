(** Representation of rna secondary structure predictions.

    This module represents the predicted secondary structure of rna
    sequences. It also contains functions for predicting secondary
    structure predictions from rna values. *)

type t
(** The abstract type of values representing an RNA sequence and its
    secondary structure. *)

val predict : Rna.t -> t
(** [get_sec_str r] is the secondary structure predictions for the rna
    [r] *)

val distance : t -> t -> int
(** [distance r1 r2] is the smallest integer [i] such that for every
    base pair [(a,b)] of [r1], there exists a base pair [(c,d)] in [r2]
    such that [Int.abs(a-c)<=i] and [Int.abs(b-d)<=i]. If either
    secondary structure has no base pairs, [distance r1 r2] is
    [Int.max_int]. *)

val assoc_to_array : int -> (int * int) list -> int array
(** [assoc_to_array size pairs] is the array [a]] with [a.i = j] if
    [(i,j)] or [(j,i)] in [pairs] and remaining entries are [-1]. *)

val get_seq : t -> string
(** [get_seq r] is the string of bases stored in the sequence of [r]. *)

val get_name : t -> string
(** [get_name r] is the name of [r]. *)

val get_pairs : t -> int array
(** [get_pairs r] is an array of base pairs in [r]. The value at index
    [i] is the index of the base pairing with [i] or -1 if [i] does not
    pair. *)

val make_t : string -> int array -> string -> bool option -> t
(** [make t] creates Secondary.t (dirty) *)