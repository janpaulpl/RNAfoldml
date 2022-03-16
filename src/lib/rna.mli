(** Representation of rna sequences.

    This module represents the data stored in the entries of rna fasta
    files, including sequence names, sequence information, and rna
    sequences. It handles loading of that data from FASTA as well as
    querying the data. *)

type t = {
  seq : string;
  name : string;
  info : string;
}
(** The abstract type of values representing a valid RNA sequence
    (containing characters 'A', 'G', 'C', 'U'), the sequence name, and
    information about it.*)

val from_fasta : string -> t list
(** [from_fasta f] is the list of rna sequences and information
    contained in fasta file [f]. If any sequences in [f] are invalid RNA
    sequences (containing characters other than 'A', 'G', 'C', 'U' or
    whitespace) these sequences are not included in [rna_from_fasta f].
    If [f] is empty returns [\[\]].

    Raises: [Invalid_argument] exception if [f] is not a readable fasta
    file.

    Effects: Prints warning message to terminal if any sequences are
    invalid. *)

val rna_from_string : string -> string -> t
(** [rna_from_string r_seq name] is the value of type [rna] with
    sequence name [name] representing the rna sequence [r_seq] and no
    information.

    Raises: [Invalid_argument] exception if [r_seq] contains characters
    other than 'A', 'G', 'C', 'U'. *)