#use "./tree.ml";;

(* 2分探索木を表すモジュールのシグネチャ *)
module type Tree_t = sig
  type ('a, 'b) t
  (* キーが'a型,値が'b型の木の型.型の中身は非公開 *)

  val empty : ('a, 'b) t
  (* 使い方 : empty *)
  (* 空の木 *)

  val insert : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
  (* 使い方 : insert tree key value *)
  (* 木 tree にキー key と値 value を挿入した木を返す *)
  (* キーがすでに存在していたら新しい値に置き換える *)

  val search : ('a, 'b) t -> 'a -> 'b
  (* 使い方  : search tree key *)
  (* 木 treeの中からキーkey対応する値を探して返す *)
  (* 見つからなければNot_found をraiseする *)
end;;

module NewTree = (Tree :Tree_t);;

NewTree.insert NewTree.empty "a" 3;;


