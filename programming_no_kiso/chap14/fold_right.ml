(* 目的 : initから始めてlstの要素を右から順にfに施しこむ *)
(* fold_right : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b *)
let rec fold_right f lst init = match lst with
    [] -> init 
  | first :: rest -> f first (fold_right f rest init);;

(* 目的 : firstとrest_resulstを加える *)
(* add_init : int -> int -> int *)
let add_int first rest_result = first + rest_result;;

(* 目的 : 受け取ったリストlstの各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = fold_right add_int lst 0;;

(* 目的 : firstは無視してrest_resultに1を加える *)
(* add_one : int -> int -> int *)
let add_one first rest_result = 1 + rest_result;;

(* 目的 : 受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let length lst = fold_right add_one lst 0;;

(* 目的 : firstをリストrest_resultの先頭に加える *)
(* cons : 'a -> 'a list -> 'a list *)
let cons first rest_result = first :: rest_result;;

(* 目的 : lst1とlst2を受け取りそれらを結合したリストを返す *)
(* append : 'a list -> 'a list -> 'a list *)
let append lst1 lst2 = fold_right cons lst1 lst2;;


