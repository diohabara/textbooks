(* 目的 : 誕生日(月と日)を受け取ったら星座を返す *)
(* seiza : int -> int -> string *)
let seiza month day = if (month = 3 && day >= 21) || (month = 4 && day <= 19) then "ohitsji"
                      else if (month = 4 && day >= 20) || (month = 5 && day <= 20) then "oshi"
                      else if (month = 5 && day >= 21) || (month = 6 && day <= 21) then "futago"
                      else if (month = 6 && day >= 22) || (month = 7 && day <= 22) then "kani"
                      else if (month = 7 && day >= 23) || (month = 8 && day <= 22) then "shishi"
                      else if (month = 8 && day >= 23) || (month = 9 && day <= 22) then "otome"
                      else if (month = 9 && day >= 23) || (month = 10 && day <= 23) then "tenbin"
                      else if (month = 10 && day >= 24) || (month = 11 && day <= 22) then "sasori"
                      else if (month = 11 && day >= 23) || (month = 12 && day <= 21) then "ite"
                      else if (month = 12 && day >= 22) || (month = 1 && day <= 19) then "yagi"
                      else if (month = 1 && day >= 20) || (month = 2 && day <= 18) then "mizugame"
                      else "uo" ;;

(* テスト *)
let test1 = seiza 9 2 = "otome" ;;
let test2 = seiza 10 1 = "tenbin" ;;
let test3 = seiza 6 18 = "futago" ;;
let test4 = seiza 2 22 = "uo" ;;
