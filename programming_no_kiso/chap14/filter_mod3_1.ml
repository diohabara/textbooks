(* 目的 : 整数nが3で割ると1余るか調べる *)
(* is_mod3_1 : int -> bool *)
let is_mod3_1 n = n mod 3 = 1;;

(* 目的 : リストlstから3で割ると1余る要素のみを取り出す *)
(* filter_mod3_1 : int list -> int list *)
let rec fitler_mod3_1 lst = match lst with
    [] -> []
  | first :: rest ->
     if is_mod3_1 first then first :: fitler_mod3_1 rest
     else fitler_mod3_1 rest;;
               
