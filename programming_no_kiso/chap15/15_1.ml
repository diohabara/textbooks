(* 本書で作ったクイックソートは同じ数で出てきた際の処理が書かれていない*)
(* take_less関数の(<)を(<=)とすることでその部分の処理を簡便にする *)

(* 目的 : 受け取ったlstをクイックソートを使って昇順に整列する *)
(* quick_sort : int list -> int list *)
let rec quick_sort lst =
  (* 目的 : lstの中からnよりpである要素のみを取り出す *)
  (* take : int -> int list -> (int -> int -> bool) -> int list *)
  let take n lst p = List.filter (fun item -> p item n) lst in
  (* 目的 : lstの中からnより小さい要素のみを取り出す *)
  (* take_less : int list -> int list *)
  let rec take_less n lst = take n lst (<=) in
  (* 目的 : lstの中からnより大きい要素のみを取り出す *)
  (* take_greater : int list -> int list *)
  let rec take_greater n lst = take n lst (>) in
  match lst with
    [] -> []
  | first :: rest -> quick_sort (take_less first rest)
                     @ [first]
                     @ quick_sort (take_greater first rest);;

(* テスト *)
let test1 = quick_sort [] = [];;
let test2 = quick_sort [1] = [1];;
let test3 = quick_sort [1; 2] = [1; 2];;
let test4 = quick_sort [2; 1] = [1; 2];;
let test5 = quick_sort [5; 4; 9; 8; 2; 3] = [2; 3; 4; 5; 8; 9];;
let test6 = quick_sort [5; 4; 4; 5; 3; 4] = [3; 4; 4; 4; 5; 5];;
