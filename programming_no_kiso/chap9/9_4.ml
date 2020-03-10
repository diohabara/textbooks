(* int list は
- [] 空リスト，あるいは
- first :: rest 最初の要素がfirstで残りのリストがrest
(rest が自己参照のケース)
という形 *)

(* 目的 : 受け取ったリストの長さを返す *)
(* length : int list -> int *)
let rec length lst = match lst with
                    [] -> 0
                  | first :: rest -> 1 + length rest ;;

(* テスト *)
let test1 = length [2; 1; 6; 4; 7] = 5 ;;
let test2 = length [0; 0; 0;] = 3;;
let test3 = length [] = 0;
