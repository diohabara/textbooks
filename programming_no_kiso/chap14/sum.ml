(* 目的 : 受け取ったリストlstの各要素の和を求める *)
(* sum : int list -> int *)
let sum lst =
  (* 目的 : firstとrest_resultを加える *)
  (* add_int : int -> int -> int *)
  let add_int first rest_result = first + rest_result in
  List.fold_right add_int lst 0;;

(* 無名関数version *)
let sum lst = 
  List.fold_right (fun first rest_result -> first + rest_result)
    lst 0;;

(* テスト *)
let test1 = sum [1;2;3;4;5] = 15;;
