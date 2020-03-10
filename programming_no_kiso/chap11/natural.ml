(* 自然数は
   - 0 0，あるいは
   - n + 1 ひとつ小さい自然数nに1を加えたもの 
           (nが自己参照のケース)
   という形 *)

(* 目的 : 自然数nの階乗を求める *)
(* fac : int -> int *)
let rec fac n =
  if n = 0 then 1
  else n * fac (n - 1) ;;

(* テスト *)
let test1 = fac 0 = 1;;
let test2 = fac 1 = 1;;
let test3 = fac 2 = 2;;
let test4 = fac 3 = 6;;
let test5 = fac 4 = 24;;
let test6 = fac 10 = 3628800;;

(* 目的 : 自然数mとnを受け取ったらmのn乗を求める *)
(* power : int -> int -> int *)
let rec power m n = 
  if n = 0 then 1 (* 0のケース *)
  else m * power m (n - 1) (* 1以上のケース *);;
                    

(* テスト *)
let test1 = power 3 0 = 1;;
let test2 = power 3 1 = 3;;
let test3 = power 3 2 = 9;;
let test4 = power 3 3 = 27;;
