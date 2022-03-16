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

val get_seq : t -> string
(** [get_seq r] is the string of bases stored in the sequence of [r]. *)

val get_name : t -> string
(** [get_name r] is the name of [r]. *)

val get_pairs : t -> int array
(** [get_pairs r] is an array of base pairs in [r]. The value at index
    [i] is the index of the base pairing with [i] or -1 if [i] does not
    pair. *)
