(* 目的 : ax^2+bx+cの判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c ;;

(* テスト *)
let test1 = hanbetsushiki 2. 3. 1. = 1. ;;
let test2 = hanbetsushiki 3. 6. 2. = 12. ;;
let test3 = hanbetsushiki 15. 24. 10. = -. 24. ;;

(* 目的 : ２次方程式が虚数解を持つかどうかを判定する *)
(* kyosuukai : float -> float -> float -> bool *)
let kyosukai a b c =
  if hanbetsushiki a b c < 0. then true
  else false ;;

(* テスト *)
let test1 = kyosukai 2. 3. 1. = false ;;
let test2 = kyosukai 15. 24. 10. = true ;;
             
