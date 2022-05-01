(** Includes functionaliyu for structured output of secondary-structure
    prediction and visualization from fasta files and structured output
    for testing of secondary structure prediction algorithms,
    constraints, and free-energy data. *)

val predict :
  ?algorithm:(Rna.t -> Secondary.t) -> ?dir:string -> string -> unit
(** [predict ?(algorithm = Nussinov.predict) ?(dir = "") f] stores
    secondary structure .ct files in directory [dir] for each rna
    sequence in fasta file [f]. Uses [algorithm] to predict secondary
    structures, if no algorithm provided uses [Nussinov.predict].

    Raises: [Invalid_argument] if [f] does not exist or is in incorrect
    fasta format.

    Effects: Prints warning message to stdout if any files are being
    overwritten. *)

(* val compare_algorithms : Rna.t list -> (Rna.t -> Secondary.t) ->
   (Rna.t -> Secondary.t) -> unit *)

val test_algorithm :
  Secondary.t list -> (Rna.t -> Secondary.t) -> float list
(** [test_algorithm sl algo] outputs a is a list of percent similarities
    between . predicts the secondary structure of RNA sequences
    associated with [sl], uses []

    Effects: Prints *)
