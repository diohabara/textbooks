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

(* 目的 : 学生リストlstのうち成績がseiseki0の人の数を返す *)
(* count : gakusei_t list -> string -> int *)
let count lst seiseki = 
  (* 目的 : 学生gak1が成績seiseki1を持つかどうか調べる *)
  (* isSeiseki : gakusei_t ->  -> bool *)
  let isSeiseki gak1 = match gak1 with
      {namae=n1;tensuu=t1;seiseki=s1} -> s1 = seiseki in
  List.length (List.filter isSeiseki lst);;

(* テスト *)
let test1 = count lst4 "B" = 1;;
let test2 = count lst4 "A" = 2;;
