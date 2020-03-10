(* 目的　: 受け取った整数m,nの最大公約数を求める *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  if n = 0 then m
  else gcd n (m mod n);;

(* テスト　*)
let test1 = gcd 5 2 = 1;;
let test2 = gcd 15 3 = 3;;
let test3 = gcd 35 14 = 7;;

(** 再起の停止性　
 * 再帰の度に引数は小さくなっている
 * 最終的には必ずnは0になるので有限回で終わる
 *)

