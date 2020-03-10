let rec fib n = 
  if n < 2 then n else fib (n-1) + fib(n-2)

(* test *)
let test1 = fib 8 = 21;;

(* obj : フィボナッチ数を再帰回数とともに求める *)
(* fib : int -> int -> (int * int) *)
let rec fib n c = (* cはこれまでに呼ばれた回数 *)
  let c0 = c + 1 in (* カウンタに1を加える *)
  if n < 2
  then (n, c0) (* カウンタを一緒に返す *)
  else let (r1, c1) = fib (n-1) c0 in (* c0からはじめてfib(n-1)中での呼び出し回数を数える *)
       let (r2, c2) = fib (n-2) c1 in (* c1からはじめてfib(n-2)中での呼び出し回数を数える *)
       (r1+r2, c2) (* c2が全体の呼び出し回数 *)

(* test *)
let test2 = fib 8 0 = (21, 67);;



(* obj : フィボナッチ数を再帰回数とともに求める *)
(* fib : int -> int -> (int * int) *)
let count = ref 0;;
!count;;
let rec fib2 n =
  (count := !count + 1;
   if n < 2 then n else fib2 (n - 1) + fib2 (n - 2));;

(* test *)
let test3 = fib2 8 = 21;;
!count
