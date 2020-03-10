(* 目的 : 予め昇順に並んでいる整数のリストlstと整数nを受け取ったら，lstを前から順に見て昇順となる位置にnを挿入する *)
(* insert int list -> int -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> if first > n then n :: first :: rest
                     else first :: insert rest n ;;

(* 目的 : 受け取った整数のリストを昇順に整列して返す *)
(* ins_sort int list -> int list *)
let rec ins_sort lst = match lst with
    [] -> []
  | first :: rest -> insert (ins_sort rest) first ;;

ins_sort [5; 3; 8; 1; 7; 4];;
ins_sort [3; 2; 1];;

(* テスト *)
let test1 = ins_sort [5; 3; 8; 1; 7; 4] = [1; 3; 4; 5; 7; 8];;
let test2 = ins_sort [3; 2; 1] = [1; 2; 3];;
let test3 = ins_sort [1; 2; 3] = [1; 2; 3];;
