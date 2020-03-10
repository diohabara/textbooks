let count = ref (-1)
(* 目的 : 受け取った文字列が呼ばれるごと異なる数字を付けた文字列を返す *)
(* gensym : string -> string *)
let gensym str = 
  (count := !count + 1;
   str ^  string_of_int !count)

(* test *)
let test1 = gensym "a" = "a0";;
let test2 = gensym "a" = "a1";;
let test3 = gensym "x" = "x2";;
