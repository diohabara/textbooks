type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;


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

(* テスト *)
let test1 = ketsueki_shukei [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;
                             {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"} ;
                             {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"}]
            = (1, 1, 1, 0);;
