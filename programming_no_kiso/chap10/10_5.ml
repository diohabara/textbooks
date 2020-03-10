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

(* 目的 : gakusei_t型のリストを受け取ったらその中から最高得点を取った人のレコードを返す *)
(* gakusei_max : gakusei_t list -> int *)
let rec gakusei_max lst = match lst with
    [] -> min_int
  | ({namae = lst_n; tensuu = lst_t; seiseki = lst_s} as first) :: rest->
     if lst_t >= gakusei_max rest
     then lst_t
     else gakusei_max rest;;


(* テスト *)
let test1 = gakusei_max[{namae="asai";tensuu=70;seiseki="B"};
                        {namae="kaneko";tensuu=85;seiseki="A"};
                        {namae="yoshida";tensuu=80;seiseki="A"}]
            = 85;;
