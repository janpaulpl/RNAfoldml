let predict ?(algorithm = Nussinov.predict) ?(dir = "") filename =
  let stripped_filename =
    filename |> Filename.basename |> Filename.remove_extension
  in
  let dir =
    if dir = "" then "structure-" ^ stripped_filename else dir
  in
  if (not (Sys.file_exists dir)) || not (Sys.is_directory dir) then
    Sys.mkdir dir 0o755;
  filename |> Rna.from_fasta
  |> List.iteri (fun i r ->
         r |> algorithm
         |> Secondary_print.to_ct
              (Filename.concat dir
                 (string_of_int i ^ "-" ^ r.name ^ ".ct")))
