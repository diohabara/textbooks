(* obj : get the gcd of two natural numbers, m, n *)
(* gcd : int -> int -> int *)
let rec gcd m n =
  (print_string "m = ";
   print_int m;
   print_string ", n = ";
   print_int n;
   print_newline ();
   if n = 0 then m
   else gcd n (m mod n))

(* test *)
let test1 = gcd 216 126 = 18;;
