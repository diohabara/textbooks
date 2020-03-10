(* 整数のリストを受け取ったら、それまでの数の合計からなるリストを返す *)
(* sum_list : int list -> int list *)
let sum_list lst =
  (* 整数のリストを受け取ったらそれまでの数の合計からなるリストを返す *)
  (* sum : これまでの値の合計 *)
  (* sum_hojo : int list -> int -> int list *)
  let rec sum_hojo lst sum = match lst with
      [] -> []
    | first :: rest ->
       (first + sum) :: sum_hojo rest (first + sum)
  in sum_hojo lst 0;;

(* テスト *)
let test1 = sum_list [3;2;1;4] = [3;5;6;10];;
