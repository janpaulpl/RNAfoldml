let list_max lst =
  List.fold_left
    (fun x y -> if List.length y > List.length x then y else x)
    [] lst

let pseudo_max seq start fin =
  (* Memmoize is a 3-dimensional array storing values for
     pseudo_max_left, pseudo_max_right, pseudo_max_mid *)
  let memoize =
    Array.init
      (fin - start + 1)
      (fun _ ->
        Array.init
          (fin - start + 1)
          (fun _ ->
            Array.init (fin - start + 1) (fun _ -> ([], [], []))))
  in

  let rec pseudo_max_left (i, j, k) =
    if k > fin || j > fin || k < j || k < start || i > fin || i < start
    then []
    else
      let left, middle, right = memoize.(i).(j).(k) in
      if left <> [] then left
      else
        let ans =
          if j = k && i >= start && i < j then
            if Secondary.is_valid_pair seq.[i] seq.[j] then [ (i, j) ]
            else []
          else if i = start - 1 && (k = j || k = j + 1) then []
          else if not (Secondary.is_valid_pair seq.[i] seq.[j]) then []
          else
            (i, j)
            :: list_max
                 [
                   pseudo_max_left (i - 1, j + 1, k);
                   pseudo_max_right (i - 1, j + 1, k);
                   pseudo_max_mid (i - 1, j + 1, k);
                 ]
        in
        memoize.(i).(j).(k) <- (ans, middle, right);
        ans
  and pseudo_max_right (i, j, k) =
    if k > fin || j > fin || k < j || k < start || i > fin || i < start
    then []
    else
      let left, middle, right = memoize.(i).(j).(k) in
      if right <> [] then right
      else
        let ans =
          if i = start - 1 && k = j + 1 then
            if Secondary.is_valid_pair seq.[i] seq.[j] then [ (i, j) ]
            else []
          else if
            (i = start - 1 && k = j)
            || not (Secondary.is_valid_pair seq.[i] seq.[j])
          then []
          else
            (i, j)
            :: list_max
                 [
                   pseudo_max_left (i - 1, j + 1, k);
                   pseudo_max_right (i - 1, j + 1, k);
                   pseudo_max_mid (i - 1, j + 1, k);
                 ]
        in
        memoize.(i).(j).(k) <- (left, middle, ans);
        ans
  and pseudo_max_mid (i, j, k) =
    if k > fin || j > fin || k < j || k < start || i > fin || i < start
    then []
    else
      let left, middle, right = memoize.(i).(j).(k) in
      if right <> [] then right
      else
        let ans =
          if i = start - 1 && (k = j + 1 || k = j) then []
          else
            list_max
              [
                pseudo_max_left (i - 1, j, k);
                pseudo_max_left (i, j + 1, k);
                pseudo_max_mid (i - 1, j, k);
                pseudo_max_mid (i, j + 1, k);
                pseudo_max_mid (i, j, k - 1);
                pseudo_max_right (i, j + 1, k);
                pseudo_max_right (i, j, k - 1);
              ]
        in
        memoize.(i).(j).(k) <- (left, middle, ans);
        ans
  in
  for t = start to fin do
    ignore (pseudo_max_left (start, t, fin));
    ignore (pseudo_max_right (start, t, fin));
    ignore (pseudo_max_mid (start, t, fin))
  done;
  memoize |> Array.to_list
  |> List.map (fun x -> Array.to_list x)
  |> List.flatten
  |> List.map (fun x -> Array.to_list x)
  |> List.flatten
  |> List.fold_left
       (fun acc (x, y, z) ->
         let lst = [ x; y; z ] in
         if List.length (list_max lst) > List.length acc then
           list_max lst
         else acc)
       []

let predict (r : Rna.t) =
  let seq = r.seq in
  let nuss =
    Nussinov.predict r |> Secondary.get_pairs |> Array.to_list
  in
  let nuss_filtered = nuss |> List.filter (fun x -> x <> -1) in
  if
    List.length (pseudo_max seq 0 (String.length seq - 1))
    >= List.length nuss - List.length nuss_filtered
  then Secondary.make r (pseudo_max seq 0 (String.length seq - 1))
  else Nussinov.predict r
