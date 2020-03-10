(* 目的 : 関数fとリストlstを受け取りfを施したリストを返す *)
(* map : ('a -> 'b) -> 'a list -> 'b list *)
let rec map f lst = match lst with
    [] -> []
  | first :: rest -> f first :: map f rest;;

;; (* exmample *)
map sqrt [2.0; 3.0];;

map sin [2.0; 3.0];;

(* 目的 : 実数のリストlstを受け取り各要素の平方根のリストを返す *)
(* map_sqrt : float list -> float list *)
let map_sqrt lst = map sqrt lst;;

(* 学生ひとり分のデータ(名前，点数，成績)を表す型) *)
type gakusei_t = {
    namae : string; (* 名前 *)
    tensuu : int; (* 点数 *)
    seiseki : string; (* 成績 *)
  };;


(*目的 : 学生のデータ gakusei を受け取り成績のついたデータを返す *)
(* hyouka : gakusei_t -> gakusei_t *)
let hyouka gakusei = match gakusei with
    {namae = n ; tensuu =t; seiseki = s} ->
    { namae = n;
     tensuu = t;
     seiseki = if t >= 80 then "A"
               else if t >= 70 then "B"
               else if t >= 60 then "C" else "D"
}

(* 目的 : 学生のリストlstを受け取り成績を入れたリストを返す *)
(* map_hyouka : gakusei_t list -> gaksuei_t list *)
let map_hyouka lst = map hyouka lst;;
                     
