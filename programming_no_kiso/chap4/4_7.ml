(* 目的 : 与えられった鶴と亀の数から足の数の合計を返す *)
(* tsurukame_no_ashi : int -> int -> int *)
let tsurukame_no_ashi tsuru kame = 2 * tsuru + 4 * kame ;;
(* テスト *)
let test1 = tsurukame_no_ashi  4 2 = 16 ;;
let test2 = tsurukame_no_ashi 3 8 = 38 ;;
let test3 = tsurukame_no_ashi 1 1 = 6 ;;
