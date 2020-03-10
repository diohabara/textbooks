type person_t = {
    namae : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;
let p1 = {namae = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;;
let p2 = {namae = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"} ;;
let p3 = {namae = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"} ;;

(* 目的 : person_t型のデータを受け取ったらその中から人の名前を返す *)
(* namae : person_t -> string *)
let namae p0 = match p0 with 
    {namae = n; m = m; kg = k; month = month; day = d; chi = c} ->
     n;;

(* 目的 : person_t型のリストを受け取って，その中から出てくる人の名前のリストを返す *)
(* person_namae : person_t list -> string *)
let rec perosn_namae lst = List.map namae lst;;

(* テスト *)
let lst1 = [p1;p2;p3];;
let test1 = perosn_namae lst1 = ["ore"; "sakura"; "bro"];;
