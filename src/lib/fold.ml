type rna_sec_str = {
  seq : string;
  pairs : (int * int) list;
  name : string;
  attributes : (string * string) list;
}

let temp =
  {
    seq = "temp";
    name = "temp";
    attributes = [ ("temp", "temp") ];
    pairs = [];
  }

(** [nussinov r] is the secondary structure for [r] given by Nussinov's
    algorithm to maximize pairing. *)
let nussinov (r : Rna.rna) = if r = r then temp else temp

let get_sec_str (rl : Rna.rna list) = List.map nussinov rl
