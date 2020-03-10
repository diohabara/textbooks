#use "./option.ml";;
(* 売り切れを示す例外 *)
exception Urikire

(* 目的 : itemの値段を調べる *)
(* 見つからない時にはUrikireという例外を発生する *)
(* price : string -> (string * int) list -> int *)
let rec price item yaoya_list = match yaoya_list with
    [] -> raise Urikire
  | (yasai, nedan) :: rest ->
     if item = yasai then nedan 
     else price item rest ;;

(* 目的 : yasai_listを買ったときの値段の合計を調べる *)
(* total_price : string list -> (string * int) list -> lnt *)
let total_price yasai_list yaoya_list =
  (* 目的 : yasai_listを買ったときの値段の合計を調べる *)
  (* hojo : string list -> int *)
  let rec hojo yasai_list = match  yasai_list with
      [] -> 0
    | first :: rest ->
       price first yaoya_list + hojo rest 
  in try
    hojo yasai_list 
  with Urikire -> 0

(* test *)
let test1 = total_price ["たまねぎ";"にんじん"] yaoya_list = 350;;
let test2 = total_price ["たまねぎ";"じゃがいも";"にんじん"] yaoya_list = 0;;
