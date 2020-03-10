(* string list は
- [] 空リスト，あるいは
- [] first::rest 最初の要素がfirstで残りのリストがrest
(restが自己参照のケース)
という形 *)

(* 目的 : 受け取った文字列のリストの要素を前からくっつけた文字列を返す *)
(* concat : string list -> string *)
let rec concat lst = match lst with
    [] -> ""
          | first :: rest -> first ^ concat rest;;

(* テスト*)
let test1 = concat ["春"; "夏"; "秋"; "冬"] = "春夏秋冬";;
let test2 = concat ["1"; "2"; "3"; "4"] = "1234";;
let test3 = concat ["人参"; "大嫌い"; "人間"] = "人参大嫌い人間";;
