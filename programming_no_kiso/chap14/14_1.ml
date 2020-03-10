(* 受け取った整数nが偶数か調べる *)
(* is_even : int -> bool *)
let is_even n = n mod 2 = 0;;

(* 受け取ったリストlstから偶数のもののみを取り出す *)
(* even : int list -> int list *)
let rec even lst = List.filter is_even lst;;

(* テスト *)
let test2 = even [3;5;1;1;1;1] = [];;
