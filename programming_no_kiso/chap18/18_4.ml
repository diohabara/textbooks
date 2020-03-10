#use "../chap9/9_10.ml";;
#use "../chap121/12_1.ml";;
#use "../chap17/17_13.ml";;
type 'a tree_t = Empty
               | Leaf of 'a
               | Node of 'a tree_t * 'a * 'a tree_t

(* obj : get the distance between 2 eki*)
(* If it cannot find, return Not=found *)
(* get_ekikan_kyori : string -> string -> ekikan_tree_t -> float *)
let rec get_ekikan_kyori eki1 eki2 tree = match tree with
    Empty -> raise Not_found
  | Node (left, k, lst, right) ->
     if eki1 < k then get_ekikan_kyori eki1 eki2 left
     else if k < eki1 then get_ekikan_kyori eki1 eki2 right
     else List.assoc eki2 lst

(* test *)
let global_ekikan_tree = inserts_ekikan Empty global_ekikan_list 
let test1 = get_ekikan_kyori "茗荷谷" "新大塚" global_ekikan_tree = 1.2 
(* let test2 = get_ekikan_kyori "茗荷谷" "池袋" global_ekikan_tree *) 
   (* Not_found を起こす *) 
let test3 = get_ekikan_kyori "東京" "大手町" global_ekikan_tree = 0.6 
