type ('a, 'b) t = int ref * (index_t * 'a * 'b) array
(* 最小値を求める値が 'a型で,その他の付加情報が'b型であるヒープの型 *)
   
val create : int -> 'a -> 'b -> ('a, 'b) t
(* 使い方 : create size key value *)
(* ヒープのサイズと'a型と'b型のダミーの値を受け取ったら *)
(* 空のヒープを返す *)

val insert : ('a, 'b) t -> 'a -> 'b -> index_t * ('a, 'b) t
(* 使い方 : insert heap key value *)
(* ヒープに新しい要素を追加する *)
(* ヒープは(破壊的に)書き換わる *)

type index_t = inf ref 
(* ヒープの添字の型 *)

val get : ('a, 'b) t -> index_t -> 'a * 'b
(* 使い方 : get heap index *)
(* ヒープのindex番目の要素を返す *)

val set : ('a, 'b) -> index_t -> 'a -> 'b -> ('a, 'b) t
(* 使い方 : set heap index key value *)
(* ヒープのindex番目の値を更新したヒープを返す *)
(* ヒープは(破壊的に)書き換わる *)

val split_top : ('a, 'b) t -> ('a * 'b) * ('a, 'b) t
(* 使い方 : split_top heap *)
(* 最小の値をもつものとそれを取り除いたヒープの値を返す *)
(* ヒープは(破壊的に)書き換わる *)
