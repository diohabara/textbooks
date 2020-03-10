(* 目的 : lst中の整数をすべて掛け合わせる *)
(* times : int list -> int *)
let rec times lst = match lst with
    [] -> 1
| first :: rest -> first * times rest ;;

(* 0が見つかったことを示す例外 *)
exception Zero

(* 目的 : lst中の整数をすべて掛け合わせる *)
(* times : int list -> int *)
let times lst =
  (* 目的 : lst中の整数をすべて掛け合わせる *)
  (* 0を見つけたら例外Zeroを起こす *)
  (* hojo : int list -> int *)
  let rec hojo lst = match lst with
      [] -> 1
    | first :: rest -> 
       if first = 0 then raise Zero
       else first * hojo rest 
  in try
    hojo lst 
  with Zero -> 0



