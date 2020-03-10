type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;
let p1 = {name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;;
let p2 = {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"} ;;
let p3 = {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"} ;;
let lst1 = [p1;p2;p3];;

(* 目的 : person_t型のリストを受け取ったら，指定された血液型の人の数を返す *)
(* count_ketsueki : person_t -> string -> int *)
let rec count_ketsueki lst chi0 = match lst with
    [] -> 0
  | {name=n;m=m;kg=kg;month=month;day=d;chi=c} :: rest ->
     if c = chi0 then 1 + count_ketsueki rest chi0
     else count_ketsueki rest chi0;;

(* テスト *)
let test1 = count_ketsueki lst1 "A" = 1;;
