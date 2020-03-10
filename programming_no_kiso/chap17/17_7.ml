(* 木を表す型 *)
type tree_t = Empty (* 空の木 *) 
                | Leaf of int (* 葉 *) 
                | Node of tree_t * int * tree_t (* 節 *);;
Node (Empty, 7, Leaf (3));;
(* receive tree_t and return the sum of node and leaf *)
(* tree_length : tree_t -> int *)
let rec tree_length tree = match tree with
    Empty -> 0
  | Leaf (n) -> 1
  | Node (t1, n, t2) -> 1 + tree_length(t1) + tree_length(t2);;

(* example of tree *)
let tree1 = Empty;;
let tree2 = Leaf(3);;
let tree3 = Node (tree1, 4, tree2);;
let tree4 = Node (tree2, 5, tree3);;

(* test *)
let test1 = tree_length tree1 = 0;;
let test2 = tree_length tree2 = 1;;
let test3 = tree_length tree3 = 2;;
let test4 = tree_length tree4 = 4;;
