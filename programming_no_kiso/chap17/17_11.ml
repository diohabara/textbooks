(* obj : receive ekimei and (ekimei, kyori) list, and return the kyori of the first ekimei *)
(* assoc : string -> (string , float) list -> float *)
let rec assoc ekimei0 lst = match lst with
    [] -> infinity
  | (ekimei, kyori) :: rest ->
     if ekimei0 = ekimei then kyori
     else assoc ekimei0 rest ;;
(* test *)
let test1 = assoc "後楽園" [("新大塚", 1.2); ("後楽園", 1.8)] = 1.8;;
let test2 = assoc "池袋" [("新大塚", 1.2); ("後楽園", 1.8)] = infinity;;
