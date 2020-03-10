let square x = x * x ;;
square 3. ;;
(* Error: This expression has type float but an expression was expected of type
   int *)

let circle = 2. *. pi *. r ;;
(* Error: Unbound value pi *)
circle 2. ;;
(* Error: Unbound value circle *)

let TV bangumi youbi =
  bangumi ^ "は" ^ youbi ^ "に放映です．" ;;
(* Error: Syntax error *)
TV "ポケモン" "木曜日" ;;
(* Error: Syntax error *)
