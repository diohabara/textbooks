(* 目的 : x座標とy座標の組を受け取って，x軸に対して対象な点の座標を返す *)
(* taisho_x : float * float -> float * float *)
let taisho_x pair = match pair with
    (x, y) -> (x, -.y) ;;

(* テスト *)
let test1 = taisho_x (1., 2.) = (1., -.2.) ;;
let test2 = taisho_x (6., 0.) = (6., 0.) ;;
let test3 = taisho_x (-.3., -4.) = (-.3., 4.) ;;
