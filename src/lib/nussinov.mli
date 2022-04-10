(** Holds Nussinov algorithm for secondary structure prediction that
    maximizes valid base pairing.*)

val predict : Rna.t -> Secondary.t
(** [precict r] is a secondary structure for [r] which maximizes the
    number of valid base pairs given by Nussinov's prediction algorithm. *)
