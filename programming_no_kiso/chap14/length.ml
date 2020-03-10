(* 目的 : 受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let length lst = List.fold_right add_one lst 0;;

(* 無名関数version *)
let length lst =
  List.fold_right (fun first rest_result -> 1 + rest_result) lst 0;;

(* テスト *)
let test1 = length [1;2;3;4;5] = 5;;
