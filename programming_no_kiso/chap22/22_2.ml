(* 目的：受け取った配列にフィボナッチ数を順に入れて返す *) 
(* fib_array : int array -> int array *) 
let fib_array array = 
  let n = Array.length array in 
  if n > 0 then array.(0) <- 0; 
  if n > 1 then array.(1) <- 1; 
  for i = 2 to n - 1 do 
    array.(i) <- array.(i - 1) + array.(i - 2); 
  done; 
  array 
 
(* テスト *) 
let test = fib_array [|0; 0; 0; 0; 0; 0; 0; 0; 0; 0|] 
	   = [|0; 1; 1; 2; 3; 5; 8; 13; 21; 34|] 
