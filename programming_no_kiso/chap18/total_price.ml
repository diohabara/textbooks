#use "./option.ml";;

(* 目的 : yasai_listを買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> int option *)
let total_price yasai_list yaoya_list = 
  (* 目的 : yasai_listを買ったときの値段の合計を調べる *)
  (* hojo : string list -> int option *)
  let rec hojo yasai_list = match yasai_list with
      [] -> Some(0)
    | first :: rest ->
       match price first yaoya_list with
         None -> None
       | Some(p) -> match hojo rest with
                      None -> None 
                    | Some (q) -> Some (p + q)
  in match hojo yasai_list with
       None -> 0
     | Some (p) -> p;;

(* test *)
let test1 = total_price ["たまねぎ" ; "にんじん"] yaoya_list = 350
let test2 = total_price ["たまねぎ" ; "じゃがいも" ; "にんじん"] yaoya_list = 0

(* ほしい野菜が必ず八百屋にあるとき *)

(* 目的 : itemの値段を調べる *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_lsit = match yaoya_list with
    [] -> 0
  | (yasai, nedan) :: rest ->
     if item = yasai then nedan
     else price item rest ;;

(* 目的 : yasai_listを買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> lnt *)
let rec total_price yasai_list yaoya_list = match yasai_list with
    [] -> 0
  | first :: rest ->
     price first yaoya_list + total_price rest yaoya_list;;

(* test *)
let test1 = total_price ["たまねぎ" ; "にんじん"] yaoya_list = 350
let test2 = total_price ["たまねぎ" ; "じゃがいも" ; "にんじん"] yaoya_list = 0
