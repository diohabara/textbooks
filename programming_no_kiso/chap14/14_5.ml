(* lengthのみをここでは書く *)
(* 目的 : 受け取ったリストlstの長さを求める *)
(* length : 'a list -> int *)
let legnth lst = 
  (* 目的 : first は無視してrest_resultに1を加える *)
  (* add_one : 'a -> int -> int *)
  let add_one first rest_result = 1 + rest_result in
  List.fold_right add_one lst 0;;

(* テスト *)
let lst1 = [1;2;3;4;5;6;7;8;9;10];;
let test1 = legnth lst1 = 10;;
