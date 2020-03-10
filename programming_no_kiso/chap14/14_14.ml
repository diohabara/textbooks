(* 受け取った文字列のリストを連結した文字列を返す *)
(* concat : string list -> string *)
let concat string_lst = List.fold_right (^) string_lst "";;

(* テスト*)
let test1 = concat ["愛";"がなんだ"] = "愛がなんだ";;
