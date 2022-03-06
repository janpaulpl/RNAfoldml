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

val write_ct : string -> rna_sec_str -> unit
(** [write_ct f r] saves the rna secondary structure [r] in connectivity
    table (.ct) format in file [f]. If [f] already exists, then replaces
    the contents of [f].

    Raises: [Invalid_argument] exception if [f] is not a valid file .ct
    file name.

    Effects: Prints warning message to terminal if [f] already exists,
    then replaces the contents of [f]. *)
