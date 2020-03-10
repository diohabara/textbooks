(* 1 *)
(* 'a -> 'a *)
let aa x = x;;

(* 2 *)
(* 'a -> 'b -> 'a *)
let aba x y = x;;

(* 3 *)
(* 'a -> 'b -> 'b *)
let abb f x = x;;

(* 4 *)
(* 'a -> ('a -> 'b) -> 'b *)
let aaba f x = x f;;

(* 5 *)
(* ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c *)
let abbcac f g x = g (f x);;
