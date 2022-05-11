(** Representation of rna secondary structure predictions.

    This module represents the predicted secondary structure of rna
    sequences. It also contains functions for predicting secondary
    structure predictions from rna values. *)

type t
(** The abstract type of values representing an RNA sequence and its
    secondary structure. *)

val is_valid_pair : char -> char -> bool
(** [is_valid_pair i j] is true if and only if characters [(i,j)] form
    one of 2 Watson-Crick RNA pairs: 'A','U' or 'C','G'. Order does not
    effect result. *)

val distance : t -> t -> int
(** [distance r1 r2] is the smallest integer [i] such that for every
    base pair [(a,b)] of [r1], there exists a base pair [(c,d)] in [r2]
    such that [Int.abs(a-c)<=i] and [Int.abs(b-d)<=i]. If either
    secondary structure has no base pairs, [distance r1 r2] is
    [Int.max_int]. Does not require [get_seq r1 = get_seq r2].

    Runs in O(nm) where [n] is the lenght of [get_seq r1] and [m] is the
    length of [get_seq r2]. *)

val similarity : t -> t -> float
(** [similarity r1 r2] is the portion of the bases in [r1] and [r2] that
    are paired to the same position.

    Runs in O(n) where [n] is the length of the rna sequence of [r1].

    Raises: [Invalid_argument] if r1 and r2 sequences do not have equal
    length. *)

val is_simple_pknot : int array -> bool
(** [is_simple_pknot r] is [true] if and only there exist two integers
    x,y in 1..n so that [is_simple_pknot pairs x y] is true. *)

val is_simple_pknot_arr : int array -> (int * int) list
(** [is_simple_pknot_arr] is the exhaustive list of elements [(x_i,y_i)]
    so that [is_simple_pknot pairs x_i y_i] is true for each i. *)

val is_pknot : int array -> bool
(** [has_pknot r] is true if the pairing represented by [pairs] is a
    pseudoknot i.e. there exists [i,j] and [k.l] such that [pairs.(i)] *)

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

val get_rna : t -> Rna.t
(** [get_rna r] is the rna of which [r] is a secondary structure. *)

val make : Rna.t -> (int * int) list -> t
(** [make rna pairs] is the secondary structure of [rna] with [pairs] as
    the base pairs.

    Raises: [Invalid_argument] exception if [pairs] is not a valid base
    pairing for [rna.seq]. *)
