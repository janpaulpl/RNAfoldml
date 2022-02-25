(** Representation of rna sequences.

    This module represents the data stored in the entries of rna fasta
    files, including sequence names, sequence information, and rna
    sequences. It handles loading of that data from FASTA as well as
    querying the data. *)

type rna
(** The abstract type of values representing a valid RNA sequence
    (sequence of characters ['A', 'G', 'C', 'U']), the sequence name,
    and information about it.*)

val read_rna_fasta : string -> rna list
(** [read_rna_fasta f] is the list of rna sequences and information
    contained in fasta file [f]. If any sequences in [f] are invalid RNA
    sequences (discontinuous or containing characters other than 'A',
    'G', 'C', 'U') these sequences are not included in
    [read_rna_fasta f].

    Raises: [Invalid_argument] exception if [f] is not a readable fasta
    file.

    Effects: Prints warning message to terminal if any sequences are
    invalid. *)

val rna_from_string : string -> rna
(** [rna_from_string r] is the value of type [rna] representing the rna
    sequence [r] and no information.

    Raises: [Invalid_argument] exception if [f] contains characters
    other than 'A', 'G', 'C', 'U'. *)

val get_seq : rna -> string
(** [get_seq r] is the string of bases stored in [r]. *)

val get_info : rna -> (string * string) list
(** [get_info r] is the association list of information of [r] of form
    [("info_name", "info")]. *)

val get_name : rna -> string
(** [get_name r] rna is the name of [r]. *)