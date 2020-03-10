(* obj : return the minimum value in the list *)
(* minimum : int -> int list -> int *)
let rec minimum i lst = match lst with
    [] -> i
  | first :: rest ->
     if i < first then minimum i rest
     else minimum first rest
