(* receive (int -> int) f and tree_t, and mapt all the elements by f *)
(* tree_map : (int -> int) tree_t -> tree *)
let tree_map f tree_t = match tree with
    Empty -> Empty
  | Leaf (n) -> Leaf(f(n))
  | Node (t1, n, t2) -> Node(f(t1), t(n), f(t2));;

(* test *)
(* double : int -> int *)
let double n = 2 * n;;
let test1 = tree_double tree1 = Empty;;
let test2 = tree_double tree2 = Leaf(6);;
let test3 = tree_double tree3 = Node (Empty, 8, Leaf(6));;
let test4 = tree_double tree4 = Node (Leaf(6), 10, Node (Empty, 8, Leaf(6)));;
