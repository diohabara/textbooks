type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
    seiza : string;
  };;

(* person_t list は
   - [] 空リスト，あるいは
   - [] first :: rest 最初の要素がfirstで残りのリストがrest
   (rest が自己参照のケース)
   という形 *)

(* person_t list 型のリストの例 *)
let lst1 = [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"; seiza = "otome"}];;

let lst2 = [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"; seiza = "hitsuji"};
            {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"; seiza = "kame"}];;

let lst3 = [{name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"; seiza = "uo"};
            {name = "yamada" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"; seiza = "otome"};
            {name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"; seiza = "otome"};
            {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"; seiza = "futago"}];;
         

(* 目的 : 受け取ったリスト lst の血液型A型の人の数を求める *)
(* otomeza : person_t list -> string list *)
let rec otomeza lst = match lst with
    [] -> []
  | {name = n; m = m; kg = k; month = mon; day = d; chi = c; seiza = s} :: rest
    -> if s = "otome" then n :: otomeza rest
       else otomeza rest ;;
     
(* テスト *)
let test1 = otomeza lst1 = ["ore"] ;;
let test2 = otomeza lst2 = [] ;;
let test3 = otomeza lst3 = ["yamada"; "ore"];;
