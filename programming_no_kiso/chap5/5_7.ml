(* 身長(m)と体重(kg)を与えられたら，BMI指数を計算する *)
(* bmi : float -> float -> float *)
let bmi m kg = kg /. (m ** 2.) ;;

(* 身長(m)と体重(kg)を与えられたら，BMI指数を計算し，その数値で体型を返す *)
(* taikei : float -> float -> string *)
let taikei m kg =
  if bmi m kg < 18.5 then "yase"
  else if bmi m kg < 25. then "hyojun"
  else if bmi m kg < 30. then "himan"
  else "kodohiman" ;;

(* テスト *)
let test1 = taikei 1.6 40. = "yase" ;;
let test2 = taikei 1.6 50. = "hyojun" ;;
let test3 = taikei 1.6 70. = "himan" ;;
let test4 = taikei 1.6 80. = "kodohiman" ;;
