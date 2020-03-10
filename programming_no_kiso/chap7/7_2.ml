(* 目的 : 名前(name)と成績(grade)の組を受け取ったら"nameさんの評価はgradeです"という文字列を返す *)
(* seiseki : string * string -> string *)
let seiseki (name, grade) = name ^ "さんの評価は" ^ grade ^ "です" ;;

(* テスト *)
let test1 = seiseki ("DIOHABARA", "最高") = "DIOHABARAさんの評価は最高です" ;;
let test2 = seiseki ("源氏", "最低") = "源氏さんの評価は最低です" ;;
let test3 = seiseki ("jack", "excellent") = "jackさんの評価はexcellentです" ;;
