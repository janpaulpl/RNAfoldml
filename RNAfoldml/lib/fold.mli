(** Representation of rna secondary structure predictions.

    This module represents the predicted secondary structure of rna
    sequences and associated information such as the sequence name, the
    nucleotide sequence, and any associated information. It also handles
    functions for getting secondary structure predictions from rna
    values. *)

type rna_sec_str
(** The abstract type of values representing an RNA sequence and its
    secondary structure. *)

val get_sec_str : Rna.rna list -> rna_sec_str list
(** [get_sec_str rl] is the list containing a list of secondary
    structure predictions for rna list [rl] in order (The ith structure
    in [get_sec_str rl] belongs to the rna in the ith position of [rl]). *)
