(* type triplet = | L | M | R

   let is_triplet ((i, j, k) : int * int * int) (seq : string) = if
   Secondary.is_valid_pair seq.[j] seq.[j] then Some L else if
   Secondary.is_valid_pair seq.[j] seq.[k] then Some R else if (not
   (Secondary.is_valid_pair seq.[j] seq.[j])) && not
   (Secondary.is_valid_pair seq.[j] seq.[k]) then Some M else None

   let is_below (x, y) (i, j, k) = (x <= i && y >= j) || (x >= j && y <=
   k) *)
