(** Holds Akutsu algorithm for secondary structure prediction that
    maximizes valid base pairing allowing the existence of simple
    pseudoknots*)

val predict : Rna.t -> Secondary.t
(** [precict r] is a secondary structure for [r] which maximizes the
    number of valid base pairs given by Akutsu's prediction algorithm. *)
