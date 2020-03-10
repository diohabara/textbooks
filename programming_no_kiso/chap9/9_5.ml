(* int list は
- [] 空リスト，あるいは
- first::rest 最初の要素がfirstで残りのリストがrest
(restが自己参照のケース)
という形 *)

(* 目的 : 受け取ったリストのうち偶数の要素のリストを返す *)
let rec even lst = match lst with
    [] -> [] 
  | first :: rest -> if first mod 2 = 0 then first :: even rest
                     else even rest ;;

(* テスト *)
let test1 = even [2; 1; 6; 4; 7;] = [2; 6; 4] ;;
let test2 = even [1; 2; 3; 4; 5;] = [2; 4;] ;;
let test3 = even [0; 0; 0;] = [0; 0; 0;] ;;
let test4 = even [1; 3; 5;] = [] ;;
               
