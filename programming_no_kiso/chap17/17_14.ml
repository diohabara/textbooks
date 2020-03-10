#use "17_11.ml";;
#use "17_12.ml";;
#use "17_13.ml";;
#use "../chap9/9_9.ml";;

(* get two ekimeis, ekikan_tree_t and return kyori between the two *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float*)
let rec get_ekikan_kyori ekimei1 ekimei2 ekikan_tree = match ekikan_tree with
    Empty -> infinity
  | Node (l, kiten, lst, r) ->
     if ekimei1 < kiten then get_ekikan_kyori ekimei1 ekimei2 l
     else if kiten < ekimei1 then get_ekikan_kyori ekimei1 ekimei2 r
     else assoc ekimei2 lst;;

(* テスト *) 
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list ;;
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 ;;
let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree = infinity ;;
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6 ;;
