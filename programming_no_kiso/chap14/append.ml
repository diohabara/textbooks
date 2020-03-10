(* 目的 : lst1とlst2を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let rec append lst1 lst2 = match lst1 with
    [] -> lst2
  | first :: rest -> first :: append rest lst2;;

(* 無名関数version *)
let append lst1 lst2 =
  List.fold_right (fun first rest_result -> first :: rest_result)
    lst1 lst2;;
