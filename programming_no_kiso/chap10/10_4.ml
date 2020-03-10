type person_t = {
    name : string ;
    m : float ;
    kg : float ;
    month : int;
    day : int;
    chi : string;
  };;
{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;;
{name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"} ;;
{name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"} ;;

(* 目的 : 昇順に整列済みのperson_t型のリストlstにperson_t型pを昇順になる位置に挿入したリストを返す *)
(* person_insert : person_t list -> person_t -> person_t list *)
let rec person_insert lst p = match lst with
    [] -> [p]
  | ({name = lst_n; m = lst_m; kg = lst_kg; month = lst_month; day = lst_d; chi = lst_c} as first) :: rest
        ->                                                                                                match p with
                                                                                                                   {name = p_n; m = p_m; kg = p_kg; month = p_month; day = p_d; chi = p_c}
                                                                                                              -> if p_n < lst_n then p :: lst
                                                                                                                 else first :: (person_insert rest p);;

(* 目的 : person_t型のリストを受け取ったら名前の順に整列したリストを返す *)
(* person_sort : person_t list -> person_t list *)
let rec person_sort lst = match lst with
    [] -> []
  | first :: rest -> person_insert (person_sort rest) first;;
  (* テスト *)
let test1 = person_sort [{name = "ore" ; m = 1.7 ; kg = 52.0 ; month = 9 ; day = 2; chi = "A"} ;
                         {name = "sakura" ; m = 1.5 ; kg = 40. ; month = 6 ; day = 2; chi = "B"} ;
                         {name = "bro" ; m = 1.75 ; kg = 55. ; month = 4 ; day = 12; chi = "O"} ]
            = [{name="bro";m=1.75;kg=55.;month=4;day=12;chi="O"};
               {name="ore";m=1.7;kg=52.0;month=9;day=2;chi="A"};
               {name="sakura";m=1.5;kg=40.;month=6;day=2;chi="B"}];;
