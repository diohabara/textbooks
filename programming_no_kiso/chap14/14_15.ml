(* 目的 : nから1までのリストを作る *)
(* enumerate : int -> int list *)
let rec enumerate n =
  if n = 0 then [] else n :: enumerate (n - 1);;

(* テスト *)
let test1 =  enumerate 5 = [5;4;3;2;1];;

(* 1から受け取った自然数までの合計を求める *)
(* one_to_n : int -> int *)
let one_to_n n = List.fold_right (+) (enumerate n) 0;;

(* テスト *)
let test1 = one_to_n 5 = 15;;
let test2 = one_to_n 10 = 55;;
