(* 座標を表すモジュール *)
module Zahyou = struct
  let x = 3.0
  let y = 4.0
  (* 目的 : 受け取った点(a, b)から点(x, y)までの距離を求める *)
  (* kyori : float * float -> float *)
  let kyori (a, b) =
  sqrt ((x -. a) *. (x -. a) +. (y -. b) *. (y -. b))
end;;

Zahyou.kyori(0., 0.);;
Zahyou.x;;
