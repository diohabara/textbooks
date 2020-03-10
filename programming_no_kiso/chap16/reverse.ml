(* 目的 : (lstの逆順のリスト) @ result を返す *)
(* ここでresultはこれまでの要素を逆順にしたリストを示す *)
let rec rev lst result = match lst with
    [] -> []
  | first :: rest -> rev rest (first :: result)
(* 目的 : 与えられたリストを逆順にして返す *)
(* reverse : 'a list-> 'a list *)
let reverse lst =
  (* 目的 : (lstの逆順のリスト) @ result を返す *)
  (* ここでresultはこれまでの要素を逆順にしたリストを示す *)
  let rec rev lst result = match lst with
      [] -> result
    | first :: rest -> rev rest (first :: result)
  in rev lst [];;

(* テスト *)
let test1 = reverse [1;2;3;4;5] = [5;4;3;2;1];;
