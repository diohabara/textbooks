(* 目的 与えられた鶴と亀の数の合計と，足の数の合計から鶴の数を返す*)
(* tsurukame : int -> int -> int *)
let tsurukame tsurukame ashi = (4 * tsurukame - ashi) / 2 ;;

(* テスト *)
let test1 = tsurukame 10 30 = 5 ;;
let test2 = tsurukame 80 200 = 60 ;;
let test3 = tsurukame 14 44 = 6 ;;
