type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;

(* person_t list は
   - [] 空リスト，あるいは
   - [] first :: rest 最初の要素がfirstで残りのリストがrest
   (rest が自己参照のケース)
   という形 *)

(* person_t list 型のリストの例 *)
let lst1 = [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"}];;
let lst2 = [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"};
        {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"}];;
let lst3 = [{name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"};
        {name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;
        {name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;
        {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"}];;
         

(* 目的 : 受け取ったリスト lst の血液型A型の人の数を求める *)
(* count_ketsueki_A : person_t list -> int *)
let rec count_ketsueki_A lst = match lst with
    [] -> 0
  | {name = n; m = m; kg = k; month = mon; day = d; chi = c} :: rest
    -> if c = "A" then 1 + count_ketsueki_A rest
       else count_ketsueki_A rest ;;
     
(* テスト *))
let test1 = count_ketsueki_A lst1 = 2 ;;
let test2 = count_ketsueki_A lst2 = 0 ;;
let test3 = count_ketsueki_A lst3 = 2 ;;

