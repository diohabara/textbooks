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

(* 目的 : 予め昇順に並んでいるgakusei_t型のリストlstとgakuse_t型の値gを受け取ったら，lstを前から見て昇順となる位置にgを挿入する *)
(* gakusei_insert gakusei_t lst -> gakusei_t -> gakusei_t lst *)
let rec gakusei_insert lst g = match lst with
    [] -> [g]
  | ({namae = lst_n; tensuu = lst_t; seiseki = lst_s} as first) :: rest
    -> match g with
         ({namae = goal_n; tensuu = goal_t; seiseki = goal_s})
         -> if lst_t > goal_t then g :: first :: rest
            else first :: gakusei_insert rest g;;

(* テスト *)
let test1 = gakusei_insert [{namae="takada";tensuu=80;seiseki="A"}] {namae="asai";tensuu=70;seiseki="B"} = [{namae="asai";tensuu=70;seiseki="B"};{namae="takada";tensuu=80;seiseki="A"}];;

(* 目的 : 受け取ったgakusei_t型のリストをtensuuフィールドの順に昇順に整列したリストを返す *)
(* gakusei_sort : gakusei_t list -> gakusei_t list *)
let rec gakusei_sort lst  = match lst with
    [] -> []
  | first :: rest -> gakusei_insert (gakusei_sort rest) first;;

(* テスト *)
let test1 = gakusei_sort[{namae="asai";tensuu=70;seiseki="B"};
                         {namae="kaneko";tensuu=85;seiseki="A"};
                         {namae="yoshida";tensuu=80;seiseki="A"}]
            = [{namae="asai";tensuu=70;seiseki="B"};
               {namae="yoshida";tensuu=80;seiseki="A"};
               {namae="kaneko";tensuu=85;seiseki="A"}];;
