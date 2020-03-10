(* 受け取ったリストから偶数のみを取り出す *)
(* even : int list -> int list *)
let even lst = 
  List.filter (fun n -> n mod 2 = 0) lst;;

(* テスト *)
let test1 = even [1;2;3;4;5;6;7;8;9] = [2;4;6;8];;

