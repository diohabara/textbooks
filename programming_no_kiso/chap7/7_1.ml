(* 目的 : 国語，数学，英語，理科，社会の点数を与えられたらその合計点と平均点を組にして返す *)
(* goukei_to_heikin : float -> float -> float -> float -> float -> float * float *)
let goukei_to_heikin kokugo sugaku eigo rika shakai =
  (kokugo +. sugaku +. eigo +. rika +. shakai, (kokugo +. sugaku +. eigo +. rika +. shakai) /. 5.) ;;

(* テスト *)
let test1 = goukei_to_heikin 50. 40. 30. 60. 20. = (200., 40.) ;;
let test2 = goukei_to_heikin 60. 50. 40. 70. 30. = (250., 50.) ;;

