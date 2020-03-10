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

(* gakusei_t list 型のデータの例 *)
let lst1 = [];;
let lst2 = [{namae = "asai"; tensuu = 70; seiseki = "B"}];;
let lst3 = [{namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}];;
let lst4 = [{namae = "yoshida"; tensuu = 80; seiseki = "A"};
            {namae = "asai"; tensuu = 70; seiseki = "B"};
            {namae = "kaneko"; tensuu = 85; seiseki = "A"}];;

(* 目的 : 学生の成績がAであるか調べる *)
(* is_A : gakusei_t -> bool *)
let is_A g0 = match g0 with
    {namae = n; tensuu; seiseki = s} ->
    s = "A";;


let rec count_A lst = List.length (List.filter is_A lst);;

(* テスト *)
let test1 = count_A lst1 = 0;;
let test2 = count_A lst2 = 0;;
let test3 = count_A lst3 = 1;;
let test4 = count_A lst4 = 2;;

