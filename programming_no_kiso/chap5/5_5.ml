(* 目的 : ax^2+bx+cの判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c ;;

(* テスト *)
let test1 = hanbetsushiki 2. 3. 1. = 1. ;;
let test2 = hanbetsushiki 3. 6. 2. = 12. ;;
let test3 = hanbetsushiki 15. 24. 10. = -. 24. ;;

(* 目的 : ２次方程式の係数が与えられたときに解の個数を返す *)
(* kai_no_kosuu : float -> float -> float -> int *)
let kai_no_kosuu a b c =
  if hanbetsushiki a b c > 0. then 2
  else if hanbetsushiki a b c = 0. then 1
  else 0 ;;

(* テスト *)
let test1 = kai_no_kosuu 3. 2. 1. = 0 ;;
let test2 = kai_no_kosuu 2. 3. 1. = 2 ;;
let test3 = kai_no_kosuu 2. 4. 2. = 1 ;;
                                    
                                     
                                    
                           
                              
