(** Includes functionality for structured output of secondary-structure
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

val test_algorithm :
  (Rna.t -> Secondary.t) -> Secondary.t list -> float list
(** [test_algorithm sl algo] outputs a list of the percent similarities
    between the secondary structure in [sl] and the secondary structures
    predicted by [algo] on the same RNA sequences. *)

val compare_algorithms :
  (Rna.t -> Secondary.t) ->
  (Rna.t -> Secondary.t) ->
  Rna.t list ->
  float list
(** [compare_algorithms rl algo1 algo2] outputs a list of the percent
    similarities between the secondary structures predicted by [algo1]
    and [algo2] for the RNA sequences in [rl]. *)
