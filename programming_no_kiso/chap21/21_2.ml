(* 整数のリストを受け取ってその中から素数なものだけを取り出す　*)
(* sieve : int list -> int list*)
let rec sieve lst = 
  (print_int (List.length lst);
   print_newline ();
   match lst with
     [] -> []
   | first :: rest ->
      first :: sieve (List.filter (fun x -> x mod first <> 0) rest));;

(* test *)
let test = sieve [2;3;4;5;6;7] = [2;3;5;7];;
