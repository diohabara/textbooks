(* 目的 : nから1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n =
  if n = 0 then [] else n :: enumerate (n - 1);;

(* テスト *)
let test1 =  enumerate 5 = [5;4;3;2;1];;

(* 階乗を求める *)
(* fac : int - > int *)
let fac n = List.fold_right ( * ) (enumerate n) 1;;

(* テスト *)
let test2 = fac 10 = 3628800;;
let test3 = fac 5 = 120;;
