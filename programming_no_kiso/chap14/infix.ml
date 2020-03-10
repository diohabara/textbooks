(* 目的 : 受け取ったリストlstの各要素の和を求める *)
(* sum : int list -> int *)
let sum lst = List.fold_right (+) lst 0;;

(* テスト *)
let test1 = sum [1;2;3;4;5] = 15;;
