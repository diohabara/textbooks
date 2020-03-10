(* 学生ひとり分のデータ(名前，点数，成績)を表すデータ) *)
type gakusei_t = {
    namae : string ; (* 名前 *)
    tensuu : int; (* 点数 *)
    seiseki : string ; (* 成績 *)
  };;

(* gakusei_list は
   - [] 空リスト，あるいは
   - first :: rest 最初の要素がfirstで残りのリストがrest
(first は gakusei_t型，restが自己参照のケース
という形 *)

let lst1 = [{namae="asai";tensuu=70;seiseki="B"};
            {namae="kaneko";tensuu=85;seiseki="A"};
            {namae="yoshida";tensuu=80;seiseki="A"}]

(* 目的 : 受け取った2つの学生の得点の合計を出す *)
(* tokuten : gakusei_t -> int -> int *)
let tokuten gak1 score = match gak1 with
    {namae=n1;tensuu=t1;seiseki=s1} -> t1 + score;;

(* 目的: : 受け取った学生のリストの得点の合計を出す *)
(* gakusei_sum : gakusei_t list -> int *)
let gakusei_sum gakusei_lst = List.fold_right tokuten gakusei_lst 0;;

(* テスト *)
let test1 = gakusei_sum lst1 = 235;;
