(* 整数のリストを受け取ってその中から素数なものだけを取り出す　*)
(* sieve : int list -> int list*)
let rec sieve lst = match lst with
    [] -> []
  | first :: rest ->
     first :: sieve (List.filter (fun x -> x mod first <> 0) rest);;

(* テスト *)
let test1 = sieve [2;3;4;5;6;7;8;9;10] = [2;3;5;7];;

(* 目的 : 2からnまでの整数のリストを返す *)
(* two_to_n : int -> int list *)
let rec two_to_n n =
  let rec loop i =
    if i <= n then i :: loop (i + 1) else [] in
  loop 2;;

(* objective : receive natural number and return the prime numbers list less then the number *)
(* prime : int -> int list *)
let prime n = sieve (two_to_n n);;

(* テスト *)
let test2 = prime 10 = [2;3;5;7];;
