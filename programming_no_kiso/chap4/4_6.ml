(* 目的 : 与えられた鶴の数から，足の本数を返す *)
(* tsuru_no_ashi : int -> int *)
let tsuru_no_ashi tsuru = 2 * tsuru ;;

(* テスト *)
let test1 = tsuru_no_ashi 4 = 8 ;;
let test2 = tsuru_no_ashi 8 = 16 ;;
let test3 = tsuru_no_ashi 3 = 6 ;;
