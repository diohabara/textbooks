(* 関数fと初期値init、そしてリストlstを受け取ったら、initから初めてリストlstの要素を左から順にfに施しこむ *)
(* fold_left : ('a -> 'b -> 'a) 'a -> 'b list -> 'a *)
let rec fold_left f init lst = match lst with
    [] -> init
  | first :: rest ->
     fold_left f (f init first) rest ;;

(* test *)
let test1 = fold_left (+) 0 [1;2;3;4;5] = 15;;
