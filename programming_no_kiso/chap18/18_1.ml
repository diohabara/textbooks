#use "../chap8/8_3.ml";;

type 'a optioni = None
                | Some of 'a ;;

(* obj : a person whose blood type is A *)
(* first_A : person_t list -> person_t option *)
let rec first_A lst = match lst with
    [] -> None
  | {name=n;m=m;kg=k;month=mon;day=d;chi=c} as first:: rest ->
     if c = "A" then Some (first)
     else first_A rest;;

(* test *)
let test1 = first_A [p1;p2;p3] = Some(p1);;
