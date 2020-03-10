(* 目的 時間を受け取り午前か午後かを返す *)
(* jikan : int -> string *)
let jikan hour = if hour > 12 then "gogo"
                 else "gozen";;

(* テスト *)
let test1 = jikan 1 = "gozen" ;;
let test2 = jikan 14 = "gogo" ;;
let test2 = jikan 12 = "gozen" ;;
