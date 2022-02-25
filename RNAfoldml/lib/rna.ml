type rna = {
  seq : string;
  name : string;
  attributes : (string * string) list;
}

let read_rna_fasta s =
  let x : rna list =
    [ { seq = ""; name = "sdas"; attributes = [ ("ad", "sds") ] } ]
  in
  if s = "" then x else x

let rna_from_string s =
  let x : rna =
    { seq = ""; name = "sdas"; attributes = [ ("ad", "sds") ] }
  in
  if s = "" then x else x

let get_seq r = r.name
let get_info r = r.attributes
let get_name r = r.name
