type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;

let person1 = {name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"};;
let person2 = {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"};;
let person3 = {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"};;

(* 目的 : person_t型のリストを受け取ったら，各血液型の人間が何人いるかを組みにして返す *)
(* ketsueki_shukei : person_t list -> int * int * int * int *)
let rec ketsueki_shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {name = n; m = m; kg = k; month = month; day = d; chi = c} :: rest ->
     let (a, b, o, ab) = ketsueki_shukei rest in
     if c = "A" then (a+1, b, o, ab)
     else if c = "B" then (a, b+1, o, ab)
     else if c = "O" then (a, b, o+1, ab)
     else (a, b, o, ab+1);;
   
(* 目的 : person_t型のデータのリストを受け取ったら，4つの血液型のうち最も人数が多かった血液型を返す *)
(* saita_ketsueki : person_t list -> string *)
let saita_ketsueki lst =
  let (a, b, o, ab) = ketsueki_shukei lst in
  if a >= b && a >= o && a >= ab then "A"
  else if b >= a && b >= o && b >= ab then "B"
  else if o >= a && o >= b && o >= ab then "O"
  else "AB"

(* テスト *)        
let test1 = saita_ketsueki [person1; person2; person3] = "A";;
