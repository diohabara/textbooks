(* 目的 : ax^2+bx+cの判別式の値を返す *)
(* hanbetsushiki : float -> float -> float -> float *)
let hanbetsushiki a b c = b ** 2. -. 4. *. a *. c ;;

(* テスト *)
let test1 = hanbetsushiki 2.0 3.0 1. = 1. ;;
let test2 = hanbetsushiki 3. 6. 2. = 12. ;;
let test3 = hanbetsushiki 15. 24. 10. = -. 24. ;;
