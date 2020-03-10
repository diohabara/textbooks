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

(* person_t型のデータを受け取ったら，その名前を取り出す *)
(* fun -> person_t -> string *)
fun p -> match p with {name=n;m=m;kg=k;month=month;day=d;chi=c}
                        -> n;;

(* テスト *)
let test1 = (fun p -> match p with {name=n;m=m;kg=k;month=month;day=d;chi=c} -> n)
              p1 = "ore";;
let test2 = (fun p -> match p with {name=n;m=m;kg=k;month=month;day=d;chi=c} -> n)
              p2 = "sakura";;
let test3 = (fun p -> match p with {name=n;m=m;kg=k;month=month;day=d;chi=c} -> n)
              p3 = "bro";;
