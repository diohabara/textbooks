(* 実際に試してみる *)
(* twice : ('a -> 'a) -> 'a -> 'a *)
let twice f =
  let g x = f (f x) in
  g;;

(* twice2 : ('_weak1 -> '_weak1) -> '_weak1 -> '_weak1 *)
let twice2 = twice twice;;

let add x = x + 1;;
let add2 = twice2 add;;
add2 2;; (* 6 *)

(* twiceにtwiceを渡すと，twiceを4回実行するような関数が出来上がる *)
