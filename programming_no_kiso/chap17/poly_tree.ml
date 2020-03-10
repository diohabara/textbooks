(* polyphase tree *)
type 'a tree_t = Empty
               | Leaf of 'a
               | Node of 'a tree_t * 'a * 'a tree_t;;

(* associative tree *)
type ('a, 'b) tree_t = 
  Empty
| Leaf of 'a * 'b
| Leaf of ('a, 'b) tree_t * 'a * 'b * ('a, 'b) tree_T;;
