(* 目的 : 予め昇順に並んでいる整数のリストlstと整数nを受け取ったら，lstを前から順に見て昇順となる位置にnを挿入する *)
(* insert int list int -> int list *)
let rec insert lst n = match lst with
    [] -> [n]
  | first :: rest -> if first > n then n :: first :: rest
                     else first :: insert rest n;;

(* テスト *)
let test1 = insert [1; 3; 4; 7; 8] 5 = [1; 3; 4; 5; 7; 8];;
let test2 = insert [2; 3; 5; 6; 9; 10] 7 = [2; 3; 5; 6; 7; 9; 10];;
let test3 = insert [] 1 = [1];;
