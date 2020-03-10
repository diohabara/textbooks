(* 目的 : 文字ch1を文字列ch2に加える *)
(* con_chars : string -> string -> string *)
let con_chars ch1 ch2 = ch1 ^ ch2;;

(* 目的 : 文字列のリストlstを順に全部前からくっつけた文字列を返す *)
(* concat : string list -> string *)
let concat lst = List.fold_right con_chars lst "";;

(* テスト*)
let lst1 = ["春"; "夏"; "秋"; "冬"];;
let lst2 = ["1"; "2"; "3"; "4"];;
let lst3 = ["人参"; "大嫌い"; "人間"];;
let test1 = concat lst1 = "春夏秋冬";;
let test2 = concat lst2 = "1234";;
let test3 = concat lst3 = "人参大嫌い人間";;
