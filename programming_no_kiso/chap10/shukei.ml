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

(* 目的 : 学生リストlstのうち各成績の人数を集計する *)
(* shukei : gakusei_t list -> int * int * int * int *)
let rec shukei lst = match lst with
    [] -> (0, 0, 0, 0)
  | {namae = n; tensuu = t; seiseki = s} :: rest ->
     let (a, b, c, d) = shukei rest in
     if s = "A" then (a+1, b, c, d)
     else if s = "B" then (a, b+1, c, d)
     else if s = "C" then (a, b, c+1, d)
     else (a, b, c, d+1);;

(* テスト *)
let test1 = shukei [{namae="asai";tensuu=10;seiseki="D"};
                    {namae="takeda";tensuu=80;seiseki="A"};
                    {namae="tokuda";tensuu=70;seiseki="B"};
                    {namae="sakura";tensuu=85;seiseki="A"};
                    {namae="nakamura";tensuu=60;seiseki="C"}]
            =(2, 1, 1, 1);;
                                              
