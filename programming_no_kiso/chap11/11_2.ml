(* 目的 : 教科書(p.111)の漸化式で定義されている数列の第n項を返す *)
(* kansu_a : int -> int *)
let rec kansu_a n =
  if n = 0 then 3
  else 2 * kansu_a (n - 1) - 1;;

(* テスト *)
let test1 = kansu_a 0 = 3;;
let test2 = kansu_a 2 = 9;;
